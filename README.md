# retro-cores

Cycle-accurate classic console CPUs (Sharp SM83, Ricoh 2A03) in SpinalHDL, built the way
[VexiiRiscv](https://github.com/SpinalHDL/VexiiRiscv) is built: a core is an empty host plus a
list of plugins, and every feature, including introspection, is a plugin that injects logic into
the others.

Status: the framework layer is in place, and an SM83 subset (186 opcodes: loads, 8-bit ALU,
INC/DEC, 16-bit loads and inc/dec, JR/JR cc/JP) passes cosimulation against a reference model
on random code. To add instructions, a CPU or a plugin, see [docs/EXTENDING.md](docs/EXTENDING.md).

## Build and run

Requirements: JDK 17+ (21 tested), sbt 1.10, Verilator 5, a C++ toolchain.

```sh
sbt test                               # all Verilator sims: demo, toy CPU, SM83 cosim
sbt "runMain retro.sm83.Sm83Verilog"   # writes gen/Sm83Core.v
```

SpinalHDL comes from Maven Central (`1.15.0`, set in `build.sbt`). Nothing is vendored from
VexiiRiscv: the plugin, fiber, database and pipeline APIs it uses all live in SpinalHDL's own
`spinal.lib` and `spinal.core.fiber`.

## Design

### Plugin injection

```scala
class RetroCore extends Component {
  val database = new Database
  val host = database on (new PluginHost)
}
val core = RetroCore(Seq(new StagePipelinePlugin, new SourcePlugin(8), new AddPlugin(1, 3), ...))
```

`RetroCore` contains no logic. Each plugin is a `FiberPlugin` whose body runs in two phases:

- **setup**: find other plugins by type (`host[StagePipelinePlugin]`), publish parameters into the
  database, and `retains(...)` any lock the plugin must contribute to before it closes;
- **build**: generate hardware, then `release()` the lock.

Because elaboration is a set of cooperating fibers, plugin order in the list does not matter, and
a plugin can add logic to a stage, decoder or bus that another plugin owns without that owner
knowing it exists. Removing a feature is removing a plugin from the list.

Parameters shared between plugins are `Database.blocking` keys (`Demo.WIDTH`): one plugin sets
the value, readers block until it is set.

### Decoupled control links

`StagePipelinePlugin` (`retro.core`) owns a chain of SpinalHDL `pipeline.CtrlLink`s created
lazily by id. Any plugin can say `pp.ctrl(2)` and place logic there:

- payloads are typed keys (`Payload(UInt(8 bits))`) that the `pipeline.Builder` propagates from
  where they are written to where they are read, so stages only declare what they touch;
- `c.bypass(KEY) := ...` rewrites a payload as it passes a stage;
- `haltWhen`, `throwWhen`, `duplicateWhen` give per-stage flow control without the stage owner
  wiring valid/ready by hand. For the retro CPUs this is the hook for bus wait states, DMA
  stalls and interrupt entry.

Contributors hold `elaborationLock`; once every one has released it the stage list is frozen,
introspection plugins (holding `tapLock`) attach, and only then are the `StageLink`s and Builder
run.

The CPUs' M-cycle sequencer is **not** one of these pipelines. It is a plain FSM (opcode
register plus M-cycle counter) built on the plugin layer; see below. `CtrlLink`s stay available
for parts that really are decoupled stages, such as a PPU/APU front end or a DMA engine.

### The retro layer

These are the parts VexiiRiscv has no equivalent for:

- **`BusPlugin`**: the single external bus (`RetroBus`: one cmd per M-cycle, `rdata`, and a slave
  `stall` for wait states or RDY). It has fixed-priority masters, so DMA is just a
  higher-priority master and the CPU stalls on the cycles it loses.
- **`RegFilePlugin(spec)`**: registers, aliased pairs (`HL` = `H ## L`) and flags (`ZF` = `F[7]`)
  described as data. Each one is automatically a source and a sink of the sequencer, and each flag
  is a branch condition.
- **`SequencerPlugin`**: opcodes are data (`OpSpec` = a list of `MCycle`s, each with a bus op,
  `sink -> source` transfers, plugin keys and an optional branch). The plugin collects every spec
  and generates the decoder from `IR ## STEP` with SpinalHDL's `DecodingSpec`. One clock is one
  M-cycle; a bus stall holds the cycle and suppresses all writes.
- **`TracePlugin` + `retro.sim.Probe`**: whitebox taps, then a sim probe, then `TraceBackend`s.
  The probe emits per-cycle and per-instruction events (address, M-cycles, stalls, ordered bus
  accesses, registers after).

Why an FSM rather than a `CtrlLink` per M-cycle: an M-cycle CPU has one instruction in flight
(plus the overlapped fetch), so there are no hazards or payloads to carry between stages. Cycle
exactness then comes down to "which bus access happens in cycle n", and an FSM states that
directly. A `CtrlLink` chain would need one link per possible M-cycle and a halt on every link.
This was decided from the design rather than by building both versions. The opcode-table API
doesn't depend on the choice, so the sequencer can be swapped later without touching the ISA
plugins.

### Introspection

`WhiteboxPlugin(payloads*)` taps every stage's `down.isValid` and the listed payloads into named
`simPublic` signals (`whitebox_s<id>_<name>`), without editing the plugins it observes. A sim
reads them through `dut.host[WhiteboxPlugin].logic`. This is the first link of the
VexiiRiscv-style chain: whitebox plugin, then a sim probe, then trace backends (cosim against a
reference model, file traces, Konata).

### The demo core

`retro.demo.DemoCore` is not a CPU; it exists to prove the mechanics:

| Plugin | Stage | Role |
|---|---|---|
| `SourcePlugin(width)` | 0 | sets `WIDTH`, emits an incrementing counter, stalls under backpressure |
| `AddPlugin(at, k)` ×2 | 1, 2 | adds a constant via `bypass`; the two don't know each other |
| `HaltPlugin(at)` | 1 | `haltWhen(stall)` from an input pin |
| `SinkPlugin(at)` | 3 | drives the result out as a `Flow` |
| `WhiteboxPlugin(VALUE)` | all | introspection taps |

`DemoCoreSim` drives random halts for 400 cycles and checks that the output sequence is exactly
`n + 13` with nothing dropped or duplicated, and that the whitebox saw every stage valid.

## Layout

```
src/main/scala/retro/core/   framework: RetroCore, Bus, RegFile, Sequencer, Microcode, Trace,
                             StagePipelinePlugin, WhiteboxPlugin
src/main/scala/retro/sim/    Probe and trace backends
src/main/scala/retro/sm83/   SM83: register spec, ALU and ISA plugins, reference model
src/main/scala/retro/demo/   toy pipeline exercising the CtrlLink layer
src/test/scala/retro/toy/    toy CPU testing sequencer, pairs, flags, branches, stalls, DMA
src/test/scala/retro/sm83/   SM83 cosimulation (hand-written and random code)
docs/EXTENDING.md            adding an instruction, a plugin or a CPU
```

## Roadmap

Done: scaffold, framework layer, SM83 subset with cosim, extension guide.

Next for SM83: stack and calls (`PUSH`/`POP`/`CALL`/`RET`/`RST`), the CB-prefixed shifter and bit
operations, interrupts and HALT/STOP, the remaining loads (`LDH`, `LD (nn),SP`, `ADD HL,rr`,
`ADD SP,e`), `DAA` and the other A/flag ops. Then the 2A03 (see the guide).
