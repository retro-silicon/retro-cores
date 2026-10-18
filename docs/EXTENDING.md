# Extending retro-cores

This guide shows how to add a plugin to an existing CPU and how to add a new CPU. The SM83 in
`src/main/scala/retro/sm83/Sm83.scala` is the worked example throughout.

## The four services

| Service | Plugin | What you get from it |
|---|---|---|
| `BusService` | `BusPlugin` | `newMaster(priority)`: a port onto the single external bus |
| `RegFileService` | `RegFilePlugin(spec)` | `read(name)`, `flag(name)`, `newWrite(name)` on registers, pairs and flags |
| `SequencerService` | `SequencerPlugin` | `addOp`, `addSource`, `addSink`, `addCond`, `newKey` |
| (whitebox) | `TracePlugin` | `simPublic` taps read by `retro.sim.Probe` |

Each service has a lock (`busLock`, `regFileLock`, `seqLock`). The rule is always the same:
**retain the lock in setup, contribute in build, release.** The owner waits for every holder
before it generates hardware, so plugin order in the list never matters.

```scala
class MyPlugin extends FiberPlugin {
  val logic = during setup new Area {
    val seq = host[SequencerService]       // find services in setup
    val retainer = retains(seq.seqLock)    // hold the sequencer open
    awaitBuild()
    seq.addSource("SP+1", host[RegFileService].read("SP") + 1)  // contribute
    retainer.release()                     // let it build
  }
}
```

## How an opcode is described

An opcode is an `OpSpec`: a name, an opcode value and its M-cycles. Each `MCycle` has at most one
bus access and any number of register transfers, written as `sink -> source` by name:

```scala
OpSpec("LD A,(HL+)", 0x2A, Seq(
  MCycle.read("HL").assign("A" -> "rdata", "HL" -> "HL+1"),   // M1: read at HL
  MCycle.fetch("PC").assign("PC" -> "PC+1")))                  // M2: fetch the next opcode
```

- **Bus ops:** `MCycle.idle`, `read(addr)`, `write(addr, data)`, `fetch(addr)`. `addr` and
  `data` name sources. The last cycle must be a `fetch`, which ends the instruction: this is the
  SM83 fetch/execute overlap, and transfers in that cycle happen alongside the fetch.
- **Transfers** happen at the end of the cycle. Sources and sinks of different widths are resized.
- **Branches:** `.branchIf(cond, target)` jumps to M-cycle `target` of the same opcode at the end
  of the cycle. Conditional timing is written this way: `JR NZ,e` skips its adder cycle with
  `branchIf("ZF", 2)`.
- **Plugin keys:** `.set(key, value)` sets a control signal a plugin created with `newKey`. The
  ALU's `ALU_OP` and `ALU_SRC` work this way.

Names that exist without you doing anything:
- `rdata`: the bus read data of the current cycle (from the sequencer);
- every register, pair and flag, as both a source and a sink (from the register file);
- every flag `X`, as the branch conditions `X` and `!X`.

Everything else is contributed by plugins. SM83's `IsaPlugin` adds the IDU sources (`HL+1`,
`SP-1`, ...) and `PC+e`. `AluPlugin` adds `alu` and `alu.F`.

The sequencer checks the table while elaborating. An unknown source, sink or condition name, a
duplicate opcode, a sink assigned twice in one cycle, or a missing final fetch all fail with the
opcode's name in the error.

## Adding an instruction to the SM83

Example: `LD (nn),A` (0xEA, 4 M-cycles: read lo, read hi, write, fetch).

1. Check the sources and sinks it needs. `WZ`, `Z`, `W` and `A` already exist.
2. Add it to `Sm83.opcodes`:
   ```scala
   op("LD (nn),A", 0xEA, imm.assign("Z" -> "rdata"), imm.assign("W" -> "rdata"),
     MCycle.write("WZ", "A"), fetch)
   ```
3. Model it in `Sm83Reference.step()`:
   ```scala
   case 0xEA => val lo = imm(); val hi = imm(); write(hi << 8 | lo, a)
   ```
4. Run `sbt "testOnly retro.sm83.*"`. The random-code cosim fills memory with the implemented
   opcodes only, so the new one is exercised automatically.

If the instruction needs new hardware, such as the stack (`PUSH`/`POP`/`CALL` need `SP-1`, which
already exists) or the CB-prefixed shifter, put that hardware in its own plugin. It publishes
sources and keys, and the opcode table names them.

## Adding a feature as a plugin

Examples of things that should be a plugin rather than an edit:

- **Interrupts** (SM83 IME/IE/IF, 2A03 NMI/IRQ): a plugin that describes the dispatch sequence
  as an `OpSpec` on an opcode value outside 0x00-0xFF (`opcodeWidth = 9`). It needs one thing the
  sequencer doesn't offer yet: a way for a plugin to override the IR loaded on a fetch cycle.
- **DMA** (2A03 OAM DMA): a `BusMaster` with a priority above the CPU's. The CPU stalls by itself
  on the cycles where it loses the bus. `Toy8.StealPlugin` in the tests is a minimal example.
- **Debug and introspection**: read anything through `TracePlugin`, or write a `TraceBackend`.
  Backends get `onCycle` (every clock, with stalls) and `onRetire` (every instruction, with its
  bus accesses and the registers after it).

## Adding a new CPU (2A03 next)

1. **Register file.** Write a `RegFileSpec`. For the 6502 core this is `A, X, Y, S, P, PC` and
   temporaries, with flags `C Z I D V N` as bits of `P`.
2. **Bus.** `BusPlugin(RetroBusConfig(16, 8))`. The slave `stall` input models RDY.
3. **Sequencer.** `new SequencerPlugin(resetOpcode = ...)`. Its `illegal` cycle is the behaviour
   of opcodes you haven't described yet.
4. **ISA plugin.** Publish the adders and address-mode sources your opcode table needs
   (`zp+X`, `abs+Y`, page-cross carry as a condition), then add the `OpSpec`s. Page-crossing
   penalty cycles are a `branchIf` that skips the fix-up cycle when there is no carry.
5. **ALU plugin.** Same shape as `Sm83.AluPlugin`: keys for the operation and operand, and sources
   for the result and the new flags.
6. **Reference model and cosim.** Copy `Sm83Reference` and `Sm83CosimSim`. The probe and backends
   are CPU-independent.
7. **Toplevel.** `RetroCore(Seq(bus, sequencer, regfile, alu, isa, trace))`.

Things the 2A03 will need that the framework doesn't have yet:
- 6502 instructions overlap differently from the SM83. The last cycle of an instruction is the
  next opcode fetch for most opcodes, which fits the model. A few, such as read-modify-write
  instructions, have dummy reads and writes that are ordinary `MCycle`s.
- Decimal mode is absent on the 2A03, which simplifies the ALU.
- Interrupt polling happens on the second-to-last cycle. That needs a key the interrupt plugin
  can read, which the `newKey` mechanism already supports.

## Testing conventions

- Unit-level framework tests use a toy CPU (`src/test/scala/retro/toy`).
- CPU tests are cosimulations. The RTL and the reference model run the same memory image, and
  every retired instruction must match in address, opcode, M-cycle count, ordered bus accesses
  and architectural registers.
- Run with random wait states too. Stalls must never change results, only timing.
