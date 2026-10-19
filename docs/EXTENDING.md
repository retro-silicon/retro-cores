# Extending retro-cores

This guide shows how to add an instruction, a plugin, a whole new CPU, a peripheral or a
console. The SM83 in `src/main/scala/retro/sm83/Sm83.scala` and the Game Boy in
`src/main/scala/retro/gb/GameBoy.scala` are the worked examples.

## The services

| Service | Plugin | What you get from it |
|---|---|---|
| `BusService` | `BusPlugin` | `newMaster(priority)` and `newSlave(mapping, priority)`: ports onto the bus |
| `RegFileService` | `RegFilePlugin(spec)` | `read`, `flag`, `newWrite` on `Register`/`Pair`/`Flag` objects; `setReset` |
| `IrqService` | a console's interrupt controller (`retro.gb.InterruptControllerPlugin`) | `request(line)`: a `Bool` to pulse |
| `DirectRead` | memories (`RamPlugin`, `retro.gb.CartridgePlugin`) | a read port that bypasses the bus, for a DMA that runs alongside the CPU (`host.list[DirectRead]`) |
| `SequencerService` | `SequencerPlugin` | `addOp`, `addSource`, `addSink`, `addCond`, `addKey`/`ctrl`, `addFetchOverride`, `fetchFire` |
| (whitebox) | `TracePlugin` | `simPublic` taps read by `retro.sim.Probe` |

Each service has a lock (`busLock`, `regFileLock`, `seqLock`, `irqLock`). The rule is always the same:
**retain the lock in setup, contribute in build, release.** The owner waits for every holder
before it generates hardware, so plugin order in the list never matters.

```scala
object Stack { val SP_PLUS_2 = Value("SP+2") }   // declared once, next to its owner

class StackPlugin extends FiberPlugin {
  val logic = during setup new Area {
    val seq = host[SequencerService]       // find services in setup
    val retainer = retains(seq.seqLock)    // hold the sequencer open
    awaitBuild()
    seq.addSource(Stack.SP_PLUS_2, host[RegFileService].read(Sm83Regs.SP) + 2)
    retainer.release()                     // let it build
  }
}
```

## The objects an opcode names

Opcode specs never use strings. Everything they mention is a Scala object declared once:

| Kind | Declared as | Hardware provided by | Example |
|---|---|---|---|
| Register, pair, flag | `Register`, `Pair`, `Flag` in a `RegFileSpec` | `RegFilePlugin`, automatically | `Sm83Regs.HL`, `Sm83Regs.ZF` |
| Computed value | `Value(name)` | `seq.addSource(value, hw)` | `Alu.RESULT`, `Idu.inc(HL)` |
| Bus read data | `SequencerPlugin.RDATA` | the sequencer | |
| Branch condition | `Cond(name)` | `seq.addCond(cond, hw)`; flags give `isSet`/`isClear` | `Interrupts.PENDING` |
| Control signal | `CtrlKey(name, width)` | `seq.addKey(key)`, read with `seq.ctrl(key)` | `Alu.OP` |

Put a plugin's objects in a companion `object` next to it (`Alu`, `Shifter`, `Interrupts`, `Idu`,
`Isa` for the SM83). A misspelt name is then a compile error. An object that no plugin provides
fails elaboration with a message naming the opcode that used it.

The `name` string on each object is only for traces and error messages.

## How an opcode is described

An opcode is an `OpSpec`: a mnemonic, an opcode value and its M-cycles, one `MCycleSpec` each.
Each `MCycleSpec` has at most one bus access and any number of register transfers, written
`sink -> source`:

```scala
import MCycleSpec.{idle, read, write}
import SequencerPlugin.RDATA

op("LD A,(HL+)", 0x2A,
  read(HL).assign(A -> RDATA, HL -> Idu.inc(HL)),   // M1: read at HL
  fetch)                                            // M2: fetch the next opcode
```

- **Bus ops:** `MCycleSpec.idle`, `read(addr)`, `write(addr, data)`, `fetch(addr)`, and
  `prefix(addr, page)`. A `fetch` ends the instruction. This is the SM83 fetch/execute overlap,
  so transfers in that cycle happen alongside the fetch. A `prefix` loads the second byte of a
  prefixed opcode into IR on the given page (SM83 `CB xx` is `0x100 | xx`). The last cycle must be
  a fetch or a prefix. An earlier cycle may also fetch, if a branch leads to it.
- **Transfers** happen at the end of the cycle. Sources and sinks of different widths are resized.
- **Branches:** `.branchIf(cond, target)` jumps to M-cycle `target` of the same opcode at the end
  of the cycle. Conditional timing is written this way: `JR NZ,e` skips its adder cycle with
  `branchIf(ZF.isSet, 2)`. `HALT` loops on one cycle with `branchIf(Interrupts.NOT_PENDING, 1)`.
- **Control keys:** `.set(key, value)` sets a `CtrlKey` for that cycle. Keys left unset take their
  default value.

The sequencer checks the table while elaborating. A duplicate opcode, a sink assigned twice in
one cycle, a missing final fetch, or an object no plugin provides all fail with the opcode's name.

## Adding an instruction to the SM83

Every documented SM83 opcode is already in the table. As an example of the procedure, here is how
`LD (nn),A` (0xEA, 4 M-cycles: read lo, read hi, write, fetch) is written:

1. Check that the objects exist. `Z`, `W`, `WZ` and `A` are in `Sm83Regs`.
2. Add it to `Sm83.opcodes`:
   ```scala
   op("LD (nn),A", 0xEA, imm.assign(Z -> RDATA), imm.assign(W -> RDATA), write(WZ, A), fetch)
   ```
3. Model it in `Sm83Reference.step()`:
   ```scala
   case 0xEA => val lo = imm(); val hi = imm(); write(hi << 8 | lo, a)
   ```
4. Run `sbt "testOnly retro.sm83.*"`. The random-code cosim fills memory with the implemented
   opcodes only, so the new one is exercised automatically.

If the instruction needs new hardware, declare its `Value`s and `CtrlKey`s in a companion object
and provide them from a plugin. The opcode table then names them.

## Adding a feature as a plugin

Examples from the SM83:

- **Interrupts** (`InterruptPlugin`): holds IME and owns the `IME_OP` key that EI, DI and RETI set.
  On an instruction-ending fetch (`seq.fetchFire`) with an interrupt pending and IME effective, it
  uses `seq.addFetchOverride` so IR loads the dispatch opcode `0x200` instead of the fetched byte.
  The dispatch sequence is an ordinary `OpSpec` that pushes PC and jumps to `Interrupts.VECTOR`.
  The plugin talks to the SoC through `requests` (IE & IF) and `ack` (clear this IF bit).
- **DMA** (2A03 OAM DMA, or the Game Boy's): a `BusMaster` with a priority above the CPU's. The
  CPU stalls by itself on the cycles where it loses the bus. `Toy8.StealPlugin` in the tests is
  a minimal example.
- **Debug and introspection**: read anything through `TracePlugin`, or write a `TraceBackend`.
  Backends get `onCycle` (every clock, with stalls) and `onRetire` (every instruction, with its
  bus accesses and the registers after it).

## Adding a new CPU

The Ricoh 2A03 (`src/main/scala/retro/r2a03/Ricoh2A03.scala`) was the second CPU and the test of
whether the framework is reusable. It went in without changing any file in `retro.core`. The
steps, with what the 2A03 needed at each one:

1. **Registers.** Declare `Register`/`Pair`/`Flag` objects and a `RegFileSpec`. The 2A03 has
   `A X Y SP P`, PC as the pair `PCH:PCL`, the address temporaries `ADH:ADL` (pair `AD`) and the
   data latch `D`, with the flags `C Z I D V N` as bits of `P`. Avoid names that clash with
   `spinal.core` (`S`, `B`, `U`, `Reg`): the stack pointer is `SP` for that reason.
2. **Bus.** `BusPlugin(RetroBusConfig(16, 8))`. The slave `stall` input models RDY.
3. **Sequencer.** `new SequencerPlugin(opcodeWidth, resetOpcode = ..., illegal = ...)`. The 2A03
   uses 9 bits: the 256 opcodes, the interrupt dispatch at 0x100 and the reset sequence at 0x101.
   `resetOpcode` is the reset sequence itself (7 cycles, then the fetch at the vector).
4. **Unit objects and plugins.** `Alu` (`OP`/`SRC` keys, `RESULT` and one value per flag),
   `Agu` (address generation: `ZP`, `STACK`, indexing with a no-carry `Cond`), `Interrupts`,
   `Isa`. One plugin per unit provides the hardware.
5. **Opcode table.** Generate it from the encoding where the encoding is regular: the 2A03 builds
   addressing modes once (`addressed(mode, kind)`) and combines them with operand uses (read,
   write, read-modify-write). Page-crossing penalty cycles are a `branchIf` that skips the fix-up
   cycle when there is no carry. Dummy reads and writes are ordinary cycles. 6502-style overlap
   (the result is written while the next opcode is fetched) is a transfer on the final `fetch`.
   Sinks are resized, so `PCL -> Agu.PC_REL` takes the low byte of a 16-bit value.
6. **Interrupts.** A plugin with `addFetchOverride`. The 6502 polls in the second-to-last cycle:
   a poll register updated every completed cycle, and a second one behind it, give the fetch the
   poll of the cycle before the previous one. Exceptions are a `CtrlKey` on the cycle concerned
   (`HOLD_POLL` on a branch's operand read, `SELECT` where the vector is chosen and an NMI can
   hijack a BRK). Where the flag changes matters as much: CLI and SEI change I on their dummy
   read, PLP and RTI as P is read off the stack.
7. **Tests.** Per-cycle single-instruction tests (SingleStepTests) when they exist, a known
   program with a reference log (nestest), a reference model written independently of the opcode
   table (`R2A03Reference`, itself checked against the single-step tests) cosimulated with the RTL
   on random code with random interrupts (`R2A03CosimSim`), and directed tests for documented
   corner cases (`R2A03InterruptSim`). Copy `R2A03SingleStepSim`: reset, run to the first fetch, load the
   state, compare every cycle. The probe and backends are CPU-independent.
8. **Toplevel.** `RetroCore(Seq(bus, sequencer, regfile, units..., isa, trace))`.

What the 2A03 showed about the framework:

- The bus, the sequencer, the register file and the trace plugin carried a second, quite
  different CPU (every cycle a bus access, no prefix page, a multi-cycle reset, interrupt polling
  before the fetch). The one addition was `SequencerService.rdata`, the bus read data as hardware,
  so a plugin can publish a value computed from it: PLP and RTI write P from the byte being read
  (bits 4 and 5 forced) in the same cycle, which their interrupt timing depends on.
- `OpSpec` requires the last cycle to be a fetch, so an instruction that never ends (JAM) carries
  an unreachable fetch after its loop.
- The single-step harnesses for the SM83 and the 2A03 are near copies; a shared one in `retro.sim`
  would remove that.

## SM83 timing notes

The opcode table follows the documented M-cycle counts and bus order. A few behaviours are the
model's own choice and have not yet been checked against hardware test ROMs:

- Interrupt dispatch takes 5 M-cycles after the discarded fetch: PC-1, SP-1, write PCH, write PCL
  and jump, then fetch at the vector. The interrupt index is latched when dispatch starts.
- HALT takes one cycle to enter, then waits until an interrupt is pending, then fetches. Entered
  with an interrupt already pending and IME off, the exit fetch does not increment PC (the HALT
  bug).
- STOP and the unused opcodes run as a 1 M-cycle NOP.

## Adding a peripheral

A peripheral is a plugin like any other. It asks the bus for a slave, declares its registers
with `RetroBusSlaveFactory` (SpinalHDL's `BusSlaveFactory` API on absolute addresses), and asks
the interrupt controller for its lines. The Game Boy plugins share this wiring through
`GbPeripheral`; here is the same thing written out:

```scala
class DividerPlugin extends FiberPlugin {
  val logic = during setup new Area {
    val bus = host[BusService]
    val irq = host[IrqService]
    val retainers = Seq(retains(bus.busLock), retains(irq.irqLock))
    awaitBuild()
    val slave = bus.newSlave(SingleMapping(0xFF04))      // answers FF04 only
    val tick = irq.request(Gb.Irq.TIMER)
    retainers.foreach(_.release())

    val div = Reg(UInt(8 bits)) init 0
    div := div + 1
    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.read(div, 0xFF04)
    regs.onWrite(0xFF04) { div := 0 }                    // factory logic is built last, so it wins
    tick := div === 0xFF
  }
}
```

- **Timing.** One clock is one M-cycle. A read is answered combinationally in the cycle it is
  issued; a write lands at the end of it (`slave.writeFire`). The slave may raise `slave.stall`
  to hold the cycle.
- **Overlap.** Where mappings overlap the highest priority wins. `OpenBusPlugin` uses priority -1
  so it only answers what nothing else claims.
- **Memory.** `RamPlugin(name, mapping, size)` decodes the low `log2Up(size)` address bits, so a
  wider mapping is a mirror (Game Boy echo RAM).
- **Pins.** A plugin can create IO from its build phase (`in Bits(8 bits)`); it becomes a pin of
  the toplevel, like `joypad_buttons` and `serial_tx` on the Game Boy.
- **CPU interface.** When a CPU-side plugin needs a SoC block, define a trait for it next to the
  CPU plugin and let the CPU plugin look it up with `host.get`, falling back to pins. That is how
  `Sm83.InterruptPlugin` finds `Sm83.InterruptController`, so the same CPU plugin list builds a
  bare core and a console.

## Adding a console

1. Start from the CPU's plugin list (`Sm83.plugins()`).
2. Add the on-chip memories (`RamPlugin`), unmapped regions (`OpenBusPlugin`), the interrupt
   controller and the peripherals. Whatever stays unmapped goes to the external bus: for the Game
   Boy, the cartridge.
3. Add a reset-state plugin if the console boots into a known state (`PostBootPlugin`).
4. Write a system reference model: the CPU reference model takes an `Sm83Bus`, so the console
   model implements `read`, `write` and `idle` (one M-cycle each), answers from its state at the
   start of the cycle, then applies the write and ticks every peripheral. `GbReference` is the
   example.
5. Cosimulate the whole system (`GameBoySim`): every retired instruction must match in M-cycles,
   bus accesses (so every peripheral register read) and CPU registers. Add known-answer tests
   for the reference model from the console's documentation (`GbReferenceSpec`), so the cosim is
   anchored to documented behaviour and not only to RTL/model agreement.

## Testing conventions

- Framework tests use a toy CPU (`src/test/scala/retro/toy`).
- CPU tests are cosimulations. The RTL and the reference model run the same memory image and see
  the same interrupt requests. Every retired instruction must match in M-cycle count, ordered bus
  accesses and architectural registers.
- Random code fills memory with implemented opcodes only, so jumps always land on code. It falls
  into loops quickly, so coverage comes from many short runs. Each suite asserts coverage of the
  base opcodes, the CB page and interrupt dispatch.
- Run with random wait states too. Stalls must never change results, only timing. (Not for the
  Game Boy system: its peripherals tick every clock, so a stall changes what they show.)
- System cosims use random code too, after a prologue that enables every interrupt source, so
  random writes reach every peripheral register and real peripherals raise the interrupts.
