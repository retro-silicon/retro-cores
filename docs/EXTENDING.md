# Extending retro-cores

This guide shows how to add an instruction, a plugin, or a whole new CPU. The SM83 in
`src/main/scala/retro/sm83/Sm83.scala` is the worked example throughout.

## The services

| Service | Plugin | What you get from it |
|---|---|---|
| `BusService` | `BusPlugin` | `newMaster(priority)`: a port onto the single external bus |
| `RegFileService` | `RegFilePlugin(spec)` | `read`, `flag` and `newWrite` on `Register`/`Pair`/`Flag` objects |
| `SequencerService` | `SequencerPlugin` | `addOp`, `addSource`, `addSink`, `addCond`, `addKey`/`ctrl`, `addFetchOverride`, `fetchFire` |
| (whitebox) | `TracePlugin` | `simPublic` taps read by `retro.sim.Probe` |

Each service has a lock (`busLock`, `regFileLock`, `seqLock`). The rule is always the same:
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

## Adding a new CPU (2A03 next)

1. **Registers.** Declare `Register`/`Pair`/`Flag` objects and a `RegFileSpec`. For the 6502 core
   that is `A, X, Y, S, P, PC` and temporaries, with flags `C Z I D V N` as bits of `P`.
2. **Bus.** `BusPlugin(RetroBusConfig(16, 8))`. The slave `stall` input models RDY.
3. **Sequencer.** `new SequencerPlugin(opcodeWidth, resetOpcode = ..., illegal = ...)`. The
   `illegal` cycle is what opcodes without a spec do.
4. **Unit objects and plugins.** For example `Alu` with `OP`/`SRC` keys and `RESULT`/`FLAGS`
   values, and an address-mode unit with `ZP_X`, `ABS_Y` and a page-cross `Cond`.
5. **Opcode table.** Page-crossing penalty cycles are a `branchIf` that skips the fix-up cycle when
   there is no carry. Read-modify-write dummy reads and writes are ordinary cycles.
6. **Reference model and cosim.** Copy `Sm83Reference` and `Sm83CosimSim`. The probe and backends
   are CPU-independent.
7. **Toplevel.** `RetroCore(Seq(bus, sequencer, regfile, units..., isa, trace))`.

Not covered by the framework yet for the 2A03: 6502 interrupt polling happens on the
second-to-last cycle rather than at the fetch. That needs a `CtrlKey` marking the polling cycle,
read by the interrupt plugin, plus the hijacking rules for NMI during BRK.

## SM83 timing notes

The opcode table follows the documented M-cycle counts and bus order. A few behaviours are the
model's own choice and have not yet been checked against hardware test ROMs:

- Interrupt dispatch takes 5 M-cycles after the discarded fetch: PC-1, SP-1, write PCH, write PCL
  and jump, then fetch at the vector. The interrupt index is latched when dispatch starts.
- HALT takes one cycle to enter, then waits until an interrupt is pending, then fetches. Entered
  with an interrupt already pending and IME off, the exit fetch does not increment PC (the HALT
  bug).
- STOP and the unused opcodes run as a 1 M-cycle NOP.

## Testing conventions

- Framework tests use a toy CPU (`src/test/scala/retro/toy`).
- CPU tests are cosimulations. The RTL and the reference model run the same memory image and see
  the same interrupt requests. Every retired instruction must match in M-cycle count, ordered bus
  accesses and architectural registers.
- Random code fills memory with implemented opcodes only, so jumps always land on code. It falls
  into loops quickly, so coverage comes from many short runs. Each suite asserts coverage of the
  base opcodes, the CB page and interrupt dispatch.
- Run with random wait states too. Stalls must never change results, only timing.
