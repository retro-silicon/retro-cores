# retro-cores

Cycle-accurate classic console CPUs (Sharp SM83, Ricoh 2A03) in SpinalHDL, built the way
[VexiiRiscv](https://github.com/SpinalHDL/VexiiRiscv) is built: a core is an empty host plus a
list of plugins, and every feature, including introspection, is a plugin that injects logic into
the others.

Status: the framework layer is in place and the SM83 implements the full documented opcode set
(243 base opcodes plus the 256 CB ops), interrupts, EI/DI/RETI, HALT and the HALT bug. It passes
cosimulation against a reference model on random code with random interrupts and wait states.
STOP is not modelled.

CPUs and SoCs are generated the same way: a Game Boy is the SM83's plugin list plus SoC plugins
(on-chip RAMs, IF/IE, timer, serial, LCD timing, joypad) in the same host. The whole system passes
cosimulation against a system reference model. To add instructions, a CPU, a peripheral or a
console, see [docs/EXTENDING.md](docs/EXTENDING.md).

## Build and run

Requirements: JDK 17+ (21 tested), sbt 1.10, Verilator 5, a C++ toolchain.

```sh
sbt test                               # all Verilator sims: demo, toy CPU, SM83 and Game Boy cosim
sbt "runMain retro.sm83.Sm83Verilog"   # writes gen/Sm83Core.v (the bare CPU)
sbt "runMain retro.gb.GameBoyVerilog"  # writes gen/GameBoy.v (CPU + SoC, cartridge bus external)
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
  higher-priority master and the CPU stalls on the cycles it loses. It also decodes on-chip slaves (see
  *CPU and SoC in one host*).
- **`RegFilePlugin(spec)`**: registers, aliased pairs (`HL` = `H ## L`) and flags (`ZF` = `F[7]`)
  described as data. Each one is automatically a source and a sink of the sequencer, and each flag
  gives the branch conditions `isSet` and `isClear`.
- **`SequencerPlugin`**: opcodes are data. An `OpSpec` is a list of `MCycleSpec`s, each with a bus
  op, `sink -> source` transfers, control keys and an optional branch. The plugin collects every
  spec and generates the decoder from `IR ## STEP` with SpinalHDL's `DecodingSpec`. One clock is
  one M-cycle; a bus stall holds the cycle and suppresses all writes. Prefix pages (SM83 `CB`) and
  a fetch override (interrupt dispatch) extend the opcode space.
- **Typed operands**: everything a spec names is an object declared once by its owner, the way
  VexiiRiscv declares `SrcKeys` and `Payload`s. Registers, pairs and flags are `Register`, `Pair`
  and `Flag` objects in a `RegFileSpec`; computed values are `Value`s, conditions `Cond`s and
  control signals `CtrlKey`s, kept in the companion object of the plugin that provides them
  (`Alu.RESULT`, `Idu.inc(HL)`, `Interrupts.PENDING`, `Shifter.OP`). A typo does not compile; an
  object no plugin provides fails elaboration with the opcode's name.

  ```scala
  op("LD A,(HL+)", 0x2A, read(HL).assign(A -> RDATA, HL -> Idu.inc(HL)), fetch)
  op("JR NZ,e", 0x20, imm.assign(Z -> RDATA).branchIf(ZF.isSet, 2), idle.assign(PC -> Isa.PC_E), fetch)
  ```
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

### CPU and SoC in one host

There is no separate SoC layer. Peripherals are plugins in the same `PluginHost` as the CPU's,
found by service the same way, so one plugin list generates a bare CPU and another, longer one
generates a console:

```scala
val cpu = RetroCore(Sm83.plugins())                     // the SM83 alone, bus and IRQs as pins
val gb  = RetroCore(Sm83.plugins() ++ Seq(              // the same plugins, plus the SoC
  new RamPlugin("wram", SizeMapping(0xC000, 0x3E00), 0x2000),
  new InterruptControllerPlugin, new TimerPlugin, new SerialPlugin, new LcdTimingPlugin, ...))
```

What makes that work:

- **The bus is an interconnect.** `BusPlugin` arbitrates masters and also decodes slaves:
  `newSlave(mapping, priority)` returns a port that sees the accesses that decode to it and
  answers in the same M-cycle (or stalls it). Addresses no slave claims go to the external
  `RetroBus` port, so the bare CPU exposes its whole bus and the Game Boy only the cartridge.
- **Registers are declared with SpinalHDL's `BusSlaveFactory`.** `RetroBusSlaveFactory(slave)`
  gives a peripheral `read`, `readAndWrite`, `createReadAndWrite`, `onWrite` on absolute
  addresses, with unused bits reading as 1 where the console does that.
- **Interrupt lines are a service.** `IrqService.request(Gb.Irq.TIMER)` returns a line to pulse.
  The Game Boy's `InterruptControllerPlugin` provides it, holds IF and IE, and implements
  `Sm83.InterruptController`. The CPU's `InterruptPlugin` looks for one: present, it wires to it;
  absent, `requests` and `ack` become pins.
- **Reset state is contributed.** `RegFileService.setReset` lets `PostBootPlugin` start the CPU
  where the DMG boot ROM would leave it (PC=0100, SP=FFFE, AF=01B0...).
- **Generic blocks live in the core.** `RamPlugin` (on-chip RAM, mirrors by address masking) and
  `OpenBusPlugin` (a low-priority constant answer for unmapped I/O) serve any console.

| Game Boy plugin | Addresses | What it does |
|---|---|---|
| `RamPlugin` ×4 | 8000, C000 (+echo), FE00, FF80 | VRAM, WRAM, OAM, HRAM |
| `InterruptControllerPlugin` | FF0F, FFFF | IF, IE, `IrqService`, `Sm83.InterruptController` |
| `TimerPlugin` | FF04-FF07 | DIV/TIMA/TMA/TAC from a 16-bit counter: falling-edge ticks (DIV and TAC write glitches) and the delayed TMA reload |
| `SerialPlugin` | FF01-FF02 | internally clocked transfers (1024 M-cycles), `serial_tx` pin with each byte sent |
| `LcdTimingPlugin` | FF40-FF4B | LCDC, STAT, LY, LYC, VBlank and STAT interrupts, fixed mode lengths; no pixels yet |
| `JoypadPlugin` | FF00 | P1 matrix, `joypad_buttons` pins, joypad interrupt |
| `OpenBusPlugin` ×2 | FEA0-FEFF, FF00-FFFF | 00 and FF for everything unmapped |
| `PostBootPlugin` | | post-boot-ROM register state |

The Game Boy runs with no wait states, and the peripherals tick every clock, so a held bus
cycle would advance the timer; that matches the hardware, which never stalls the CPU.

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
src/main/scala/retro/core/   framework: RetroCore, Bus (masters, slaves, register factory), RegFile,
                             Sequencer, Microcode, Soc (IrqService, RamPlugin, OpenBusPlugin),
                             Trace, StagePipelinePlugin, WhiteboxPlugin
src/main/scala/retro/sim/    Probe and trace backends
src/main/scala/retro/sm83/   SM83: registers, ALU, shifter, interrupt and ISA plugins, reference model
src/main/scala/retro/gb/     Game Boy SoC plugins, the GameBoy composition, system reference model
src/main/scala/retro/demo/   toy pipeline exercising the CtrlLink layer
src/test/scala/retro/toy/    toy CPU testing sequencer, pairs, flags, branches, stalls, DMA
src/test/scala/retro/sm83/   SM83 cosimulation (hand-written and random code)
src/test/scala/retro/gb/     Game Boy system cosimulation, known-answer tests of the reference model
docs/EXTENDING.md            adding an instruction, a plugin or a CPU
```

## Roadmap

Done: scaffold, framework layer, the SM83 instruction set with interrupts, extension guide, CPU
and SoC unified in one plugin host, Game Boy SoC (RAMs, IF/IE, timer, serial, LCD timing, joypad)
with system cosim.

Next: run hardware test ROMs (Blargg `cpu_instrs`/`instr_timing`, Mooneye timer and interrupt
tests) on the Game Boy composition; they need a cartridge model with MBC1 in the sim and a
download route for the ROM images, which this environment's network blocks. Then OAM DMA (the
Game Boy CPU keeps running from HRAM during it, so it is not a plain higher-priority master),
the PPU, STOP, and the 2A03 (see the guide). The reference models encode timings from the Pan
Docs that have not been validated against real silicon.
