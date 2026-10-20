# retro-cores

Cycle-accurate classic console CPUs (Sharp SM83, Ricoh 2A03) in SpinalHDL, built the way
[VexiiRiscv](https://github.com/SpinalHDL/VexiiRiscv) is built: a core is an empty host plus a
list of plugins, and every feature, including introspection, is a plugin that injects logic into
the others.

Status: the framework layer is in place and the SM83 implements the full documented opcode set
(243 base opcodes plus the 256 CB ops), interrupts, EI/DI/RETI, HALT and the HALT bug. It passes
cosimulation against a reference model on random code with random interrupts and wait states,
and on the Game Boy composition it passes Blargg's `cpu_instrs`, `instr_timing`, `mem_timing`,
`mem_timing-2` and `halt_bug` and every Mooneye acceptance test for the CPU, interrupts, HALT,
timer, serial clock and OAM DMA (see *Hardware test ROMs*). STOP is not modelled. The Ricoh
2A03 (NES CPU) runs on the same framework with all 256 opcodes, and is cycle-exact on the
SingleStepTests `nes6502` set and on nestest (see *Ricoh 2A03*).

CPUs and SoCs are generated the same way: a Game Boy is the SM83's plugin list plus SoC plugins
(on-chip RAMs, IF/IE, timer, serial, PPU, joypad) in the same host. The whole system passes
cosimulation against a system reference model. To add instructions, a CPU, a peripheral or a
console, see [docs/EXTENDING.md](docs/EXTENDING.md).

## Build and run

Requirements: JDK 17+ (21 tested), sbt 1.10, Verilator 5, a C++ toolchain.

```sh
sbt test                               # all Verilator sims: demo, toy CPU, SM83 and Game Boy cosim
sbt "runMain retro.sm83.Sm83Verilog"   # writes gen/Sm83Core.v (the bare CPU)
sbt "runMain retro.gb.GameBoyVerilog"  # writes gen/GameBoy.v (CPU + SoC, cartridge bus external)
```

### Single-instruction tests

```sh
scripts/fetch-sm83-tests.sh                        # SingleStepTests/sm83 (MIT) into ./roms/sst-sm83, about 190 MB
sbt "testOnly retro.sm83.Sm83SingleStepSim"        # all 500,000 tests on the reference model and the RTL (about 2 minutes)
SST_OPS="f1,cb 46" SST_LIMIT=50 sbt "testOnly retro.sm83.Sm83SingleStepSim"  # a few opcodes
```

[SingleStepTests/sm83](https://github.com/SingleStepTests/sm83) gives 1000 tests per opcode
(every base and CB opcode): an initial CPU and RAM state, the final state, and the bus of every
M-cycle (idle, read or write, address and data). Both the RTL and `Sm83Reference` pass all of
them except HALT and STOP, 498,000 of 500,000: the tests come from ares, which runs HALT as two
idle M-cycles and STOP as three, while here HALT keeps fetching at PC (the timing the Mooneye
HALT tests pin down) and STOP is a NOP. Per-opcode reports go to `out/sm83-sst-*.txt`. The tests
raise no interrupts, so dispatch, HALT exit and the EI delay are covered by the test ROMs below.

### Ricoh 2A03 (NES CPU)

```sh
scripts/fetch-nes6502-tests.sh                     # SingleStepTests/65x02 nes6502 (MIT) into ./roms/sst-nes6502, about 1.1 GB
scripts/fetch-nes-test-roms.sh                     # nestest and its Nintendulator log into ./roms/nes
sbt "testOnly retro.r2a03.*"                       # single-step tests (1000 per opcode), nestest, cosim, interrupt checks
SST_LIMIT=10000 sbt "testOnly retro.r2a03.R2A03SingleStepSim"   # all 2,560,000 tests (about 15 minutes)
sbt "runMain retro.r2a03.R2A03Verilog"             # writes gen/R2A03Core.v
```

`retro.r2a03.Ricoh2A03` is a 6502 without decimal mode on the same framework as the SM83. All 256 opcodes are implemented, the undocumented ones included (the
unstable ANE/LXA use the constant EE; SHA/SHX/SHY/TAS corrupt the address high byte on a page
cross), plus IRQ, NMI (with BRK hijacking), reset and JAM. It passes every one of the 2,560,000
[SingleStepTests/65x02](https://github.com/SingleStepTests/65x02) `nes6502` tests cycle by cycle
(read or write, address and data of every cycle, final registers and RAM), and runs nestest in
automation mode matching all 8,991 lines of the Nintendulator log, cycle counts included.
`R2A03Reference`, a cycle-by-cycle model written independently of the opcode table, passes the
same single-step tests and is cosimulated with the RTL on random code with random IRQ and NMI
levels (`R2A03CosimSim`): every instruction must match in cycles, bus accesses and registers.
Interrupts poll in the second-to-last cycle; the CLI/SEI/PLP delay, RTI taking effect at once,
NMI edge detection and the taken-branch polling quirk follow the nesdev wiki and are also checked
by directed tests (`R2A03InterruptSim`). An NMI counts in the cycle its edge is seen, hijacks
BRK or IRQ until PCL is pushed, and the vector reads do not poll, so a handler's first
instruction always runs (blargg's cpu_interrupts_v2 pins all of this down).

### NES system model

```sh
scripts/fetch-nes-test-roms.sh                     # also blargg's CPU, PPU and APU test ROMs into ./roms/nes-test-roms
sbt "Test/runMain retro.nes.NesRefRom"             # every mapper 0/1/3/4 test ROM on the model; frames saved to out/nes/
sbt "Test/runMain retro.nes.NesRefRom sprite_hit"  # only ROMs whose path contains an argument
WAV=1 sbt "Test/runMain retro.nes.NesRefRom apu_mixer"  # also saves the sound to out/nes/*.wav
sbt "Test/runMain retro.nes.NesMixerCheck"         # apu_mixer's residual tones against hardware recordings
```

`retro.nes.NesReference` is a whole NES around `R2A03Reference`: 2 KB of RAM, a dot-level 2C02
model (`NesPpuReference`: background pipeline, sprite evaluation with the overflow bug and its
timing, sprite 0 hit, VBlank/NMI with the read-race suppression, the odd-frame dot, open-bus
decay), the whole APU (`NesApuChannels.scala`: frame counter and IRQ, length counters, two
pulses with sweep, triangle, noise, the DMC, and the non-linear mixer), the 2A03's DMA unit,
controller 1, NROM/MMC1/CNROM/MMC3 cartridges, and the reset button (`reset()`: the CPU's reset
sequence at the next instruction, 4015 cleared, 4017 rewritten). Each CPU cycle runs two PPU
dots, the APU, the CPU access, the third dot, then samples NMI and IRQ. The runners press reset
when a test asks for it (status 81 at 6000).

DMA follows the chip's DMA unit as Mesen models it: OAM DMA and DMC sample fetches halt the CPU
on its next read, repeat the halted read in the halt, dummy and alignment cycles, read on odd
cycles and write OAM on even ones, and a DMC fetch steals a read cycle from a running OAM copy.
Length counter halt flags and loads take effect after the next cycle's APU clock (blargg's
len_halt_timing and len_reload_timing). The mixer uses the nesdev wiki's exact formulas, the
triangle, noise and DMC through one table indexed by their weighted sum: the common
`3 t + 2 n + d` table weighs the triangle 9% too heavily, which apu_mixer/triangle hears.
`NesMixerCheck` runs the four apu_mixer tests, which cancel a tone with the DMC, through the
console's output filters, and checks that the residual is no louder than in the hardware
recordings in the nes-test-roms collection: square 0.8% (hardware 1.2%), triangle 1.5% (1.9%),
noise 11.6% (10.8%), DMC 0.8% (2.8%).

The model passes 138 of the 144 ROMs in `fetch-nes-test-roms.sh` (ppu_vbl_nmi, vbl_nmi_timing,
sprite_hit, sprite_overflow, cpu_interrupts_v2, instr_test, instr_timing, nes_cpu_test5,
cpu_dummy_writes, cpu_exec_space, oam_stress, ppu_open_bus, sprdma_and_dmc_dma, apu_test,
blargg_apu_2005.07.30, apu_mixer, apu_reset, cpu_reset, dmc_dma_during_read4, read_joy3,
the MMC3 tests and others; print-only tests are judged by the CRCs their source lists). The
rest of the nes-test-roms collection is demos, visual or interactive tests, PAL tests, or
needs other mappers; instr_test-v3 and nes_instr_test are older versions of instr_test-v5.
Left: LXA in instr_test 03-immediate, the all_instrs ROM that stops there and nes_cpu_test5's
cpu.nes, where blargg's console and SingleStepTests disagree on the unstable constant (the core
follows the latter).
A 2007 read in the CPU cycle right after another (an indexed read from 20F7 crossing a page)
is ignored and reads open bus, as in Mesen: one of the outcomes double_2007_read accepts, which
on hardware depend on CPU-PPU alignment.
MMC3 (mapper 4) clocks its scanline counter on PPU A12 rising edges after A12 has been low for
10 dots, with the Sharp chip's IRQ behavior (a counter reloaded to 0 fires). The cartridge
sees every PPU bus address the 2C02 drives: unused sprite slots, the garbage nametable fetches
at dots 257-320, 337 and 339, and 2006/2007 accesses outside rendering. Of blargg's
mmc3_test, mmc3_test_2 and mmc3_irq_tests (18 ROMs) three fail, which test other chips (MMC6,
MMC3 revision A and the alternate MMC3 behavior).
It is the reference the RTL NES system is cosimulated against.

### NES system (RTL)

```sh
sbt "testOnly retro.nes.NesSim"                    # six short ROMs, RTL cosimulated with NesReference
sbt "Test/runMain retro.nes.NesRomReport"          # every mapper 0/1/3/4 test ROM, cosimulated, one per core at a time
sbt "Test/runMain retro.nes.NesRomReport sprite_hit"
JOBS=2 sbt "Test/runMain retro.nes.NesRomReport"   # fewer at a time
sbt "Test/runMain retro.nes.NesSpeed"              # cycles per second: model, RTL alone, cosim
```

```sh
sbt "runMain retro.nes.NesVerilog"                 # writes gen/Nes.v (NROM cartridge, no trace taps)
scripts/synth-nes-ecp5.sh                          # Yosys and nextpnr for an ECP5-25F (DEVICE=85k etc.): size and clock
```

`retro.nes.Nes(spec)` is the 2A03 plugins plus the NES ones in one host:

| NES plugin | Addresses | What it does |
|---|---|---|
| `RamPlugin` | 0000-1FFF | 2 KB of RAM, mirrored |
| `NesPpuPlugin` | 2000-3FFF | the 2C02, dot for dot as `NesPpuReference`: one dot per fast clock (three per CPU cycle, the access between the second and third), CIRAM, OAM, palette, `ppu_pixel` port |
| `NesApuPlugin` | 4000-4017 | frame counter and IRQ, length counters, pulses, triangle, noise, DMC, mixer (`audio` port, one sample per cycle), 4015, controller 1 (`joypad1` pins), and the DMA unit (OAM DMA and DMC fetches) as a second bus master above the CPU |
| `NesCartridgePlugin` | 6000-FFFF | PRG ROM, 8 KB PRG RAM, CHR ROM or RAM, mirroring, CNROM's bank, MMC1's registers or MMC3's banks and scanline IRQ (fed every PPU bus address); images loaded by the sim |
| `NesOpenBusPlugin` | everything else | the data bus latch, for unmapped reads and undriven bits |
| `NesInterruptsPlugin` | | the CPU's NMI (from the PPU) and IRQ (from the APU and the cartridge), and the reset button (`reset_button` pin, taken at the next opcode fetch) |

`NesRtl.cosim` runs a test ROM on the RTL and the model in lockstep: every instruction must
match in cycles (DMA stalls included), bus accesses (so every PPU and APU register read) and
registers, every frame pixel for pixel, and the APU's mixed output in every cycle, until the
ROM reports its result. All 144 ROMs run to their result with no difference from the model, so
the RTL passes the same 138.

The NES runs on a clock three times the CPU's (5.37 MHz), one PPU dot per clock:
`RetroCore(plugins, clockDivider = 3)` runs the plugins on that clock enabled every third
cycle, and the PPU and MMC3's scanline counter build their logic on the fast clock
(`CoreClocks`). That lets every large memory be block RAM: the PPU reads CIRAM and CHR
synchronously, a clock ahead, addressed from the state the current dot leaves, and the CPU's
RAM, PRG ROM, PRG RAM and the mixer's table are read on the fast clock (`MidCycleRead`).

On an ECP5-25F (Yosys, nextpnr) the NES with an NROM cartridge (32 KB PRG and 8 KB CHR ROM,
built in) uses 30% of the logic cells (7.4k; Yosys maps 5.9k LUT4s), 1.6k flip-flops, 64 LUT
RAM blocks, 31 of the 56 block RAMs and 2 multipliers, with no latches, and closes at 44 MHz
against the 5.37 MHz it needs. (With the PPU running three dots per CPU clock, it was 22.0k
cells on an 85F with the ROMs left out.)

Simulation speed (`NesSpeed`): the cosim runs about 8 frames per second per core, the RTL alone
14 and the model 110, so the whole report takes about 24 minutes on four cores (it was 2 frames
per second; 10 before the PPU went to one dot per clock).
What it takes:
- `retro.sim.FastVerilator` builds the C++ at -O3 (SpinalSim's default is -O0) and emits
  SpinalSim's `simPublic` markers as `verilator public_flat_rd`. Verilator assumes a `public`
  signal may be written by the testbench between evaluations, so it re-evaluated everything
  that depends on the register file and memories on both clock edges.
- `SequencerPlugin(romDecoder = true)` decodes IR ## STEP with a ROM instead of minimized logic,
  which Verilator evaluates as one array read (the sims use it; the default stays logic).
- The NES has no external bus port (`BusPlugin(external = false)`: the open bus plugin claims
  every free address) and registers the controller pins, so no logic depends on an input.
- The probe reads each cycle's bus as one packed signal and the registers as 64-bit words.

### Hardware test ROMs

```sh
scripts/fetch-test-roms.sh                         # Blargg's CPU tests, the Mooneye Test Suite and dmg-acid2 into ./roms
sbt "testOnly retro.gb.GameBoyRomSim"              # each ROM as a test on the RTL (about 10 minutes)
sbt "Test/runMain retro.gb.GbRomReport timer/"     # pass/fail table for ROMs whose name matches
sbt "Test/runMain retro.gb.GbRefRom timer/"        # the same ROMs on the reference model, in seconds
sbt "testOnly retro.gb.GameBoyLcdSim"              # dmg-acid2 on the RTL; frames saved to out/
sbt "Test/runMain retro.gb.GbRefFrame rom.gb out.png 20"  # a ROM's 20th frame from the reference model
```

Only freely distributable test ROMs are used: Blargg's tests and the MIT-licensed Mooneye Test
Suite; `./roms` is gitignored and `GameBoyRomSim` is skipped when it is empty. The ROM runs on
`GameBoy(cartridge = ...)` with a `CartridgePlugin` built from the ROM header (ROM only, MBC1 or
MBC5). Blargg's ROMs report over serial or in cartridge RAM, Mooneye's send the Fibonacci
registers.

Result on the RTL: 114 of the 134 ROMs pass, including all of Blargg's `cpu_instrs`,
`instr_timing`, `mem_timing`, `mem_timing-2` and `halt_bug`, every Mooneye acceptance test for
CPU timing, interrupts, HALT, the timer, the serial clock and OAM DMA, eleven of the twelve PPU
tests (mode 3 lengths with objects and SCX, VRAM/OAM locking, LCD-on timing), Blargg's `oam_bug`
(the DMG OAM corruption bug), and the MBC1 and MBC5 tests. The other 19 are registered as
ignored with the reason: nine are for other models (DMG-0, MGB, SGB), MBC2 and the MBC1 multicart are not modelled, and `oam_bug`'s `7-timing_effect` alone
runs far past its budget (part 7 of the combined `oam_bug` ROM, which passes, checks the same CRC).
`intr_1_2_timing` fails since line 0's mode 2 interrupt reaches a running CPU an M-cycle late,
as Mealybug's tests need; the two are not reconciled yet.

`GameBoyLcdSim` runs [dmg-acid2](https://github.com/mattcurrie/dmg-acid2) (MIT, fetched by the
same script) and checks the RTL's frame against the test's reference image and the reference
model's frame, pixel for pixel; it writes both to `out/`.

The same script fetches the DMG-relevant suites of
[c-sp/game-boy-test-roms](https://github.com/c-sp/game-boy-test-roms) v7.0, Mealybug Tearoom
Tests included (mid-scanline PPU register writes, checked by screenshot). Test ROMs are used
whatever their license (some state none); commercial games never are.

```sh
sbt "Test/runMain retro.gb.GbSuiteReport rtl"        # every suite on the RTL; table in out/gb-suites-rtl.tsv
sbt "Test/runMain retro.gb.GbSuiteReport model m3_"  # on the reference model, filtered by name
```

`SHARD=i/n` runs every n-th ROM, for parallel JVMs (give each its own `SIM_WORKSPACE`). Results
on the RTL (DMG ROMs; CGB-only, MBC3 and button-driven ROMs are skipped):

| Suite | Pass | Fail |
|---|---|---|
| Mealybug Tearoom Tests | 15 | 9 |
| Gambatte | 1726 | 147 |
| GBMicrotest | 464 | 49 |
| wilbertpol's Mooneye | 92 | 3 |
| AGE | 7 | 7 |
| Scribbltests | 5 | 0 |
| SameSuite | 3 | 0 |
| Blargg's dmg_sound | 13 | 0 |
| Bully, little-things, Strikethrough, TurtleTests | 2 | 3 |

The pixel transfer is a port of SameBoy's DMG FIFO and fetcher, which passes the same 15
Mealybug DMG tests; `PpuReference` lists the timing it pins down (register writes landing between
a cycle's dots, mode 3 ending 3 dots before the last pixel, when a halted or running CPU sees a
STAT interrupt, LY in line 153). OAM DMA bus conflicts are modelled (`OamDmaPlugin`). The reference
model has no MBC, so the 40 Gambatte OAM DMA tests that bank-switch or push to cartridge RAM
pass only on the RTL. The APU (`ApuReference`, a port of SameBoy's `apu.c`, and `ApuPlugin`)
passes 153 of the 155 sound tests (Gambatte's, blargg's dmg_sound, SameSuite's DMG ones); Gambatte's
audio tests are judged the way its test runner does (`AudioWindows`). Most of what still fails is
one of: tests SameBoy fails too, mid-line timing that depends on the post-boot PPU phase, and
cycle-exact corner cases of HALT and of interrupts raised in an instruction's last cycle.

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
  new InterruptControllerPlugin, new TimerPlugin, new SerialPlugin, new PpuPlugin, ...))
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
| `RamPlugin` ×3 | 8000, C000 (+echo), FF80 | VRAM, WRAM, HRAM |
| `OamDmaPlugin` | FE00, FF46 | OAM and its DMA, which runs alongside the CPU and reads its source through `DirectRead` ports |
| `InterruptControllerPlugin` | FF0F, FFFF | IF, IE, `IrqService`, `Sm83.InterruptController` |
| `TimerPlugin` | FF04-FF07 | DIV/TIMA/TMA/TAC from a 16-bit counter: falling-edge ticks (DIV and TAC write glitches) and the delayed TMA reload |
| `SerialPlugin` | FF01-FF02 | internally clocked transfers, shifting on the 8192 Hz edges of the timer's counter; `serial_tx` pin with each byte sent |
| `PpuPlugin` | FF40-FF4B | the PPU: dot-level pixel FIFO (background, window, objects), `lcd` pixel port, LCD registers, VBlank and STAT interrupts |
| `JoypadPlugin` | FF00 | P1 matrix, `joypad_buttons` pins, joypad interrupt |
| `OpenBusPlugin` ×2 | FEA0-FEFF, FF00-FFFF | 00 and FF for everything unmapped |
| `PostBootPlugin` | | post-boot-ROM register state |
| `CartridgePlugin` | 0000-7FFF, A000-BFFF | optional: ROM, RAM and an MBC1 or MBC5, for sims and FPGA builds with the game in block RAM |

The Game Boy runs with no wait states, and the peripherals tick every clock, so a held bus
cycle would advance the timer; that matches the hardware, which never stalls the CPU.

Every peripheral follows one timing rule: in each M-cycle it ticks first, then the CPU's access
sees the ticked state (reads return it combinationally) and a write lands on top of it. The
CPU's interrupt check sees requests raised by that tick too. Mooneye's `div_timing`,
`intr_timing` and timer tests, and Blargg's `mem_timing`, pin this down; it is described at
`retro.gb.Gb`.

### The PPU

`PpuPlugin` is a dot-level pixel FIFO: OAM scan (two entries per M-cycle, ten objects per line),
then a background fetcher that fills an 8-pixel FIFO, an object fetcher that stalls it, and an
object FIFO that mixes by priority, so mode 3 stretches by SCX, the window and each object exactly
as on the DMG. The design runs one clock per M-cycle, so each clock chains four copies of the
one-dot step, each with its own VRAM read port. VRAM and OAM are read through their own ports
(`RamPlugin.directRead`, `OamDmaPlugin.oamRead`), not over the CPU's bus.

Pixels leave on the `lcd` port, up to four per clock with their x, y and shade, for a sim to
collect or an FPGA build to write into a scan-out framebuffer. `PpuReference` is the same
pipeline as a Scala model inside `GbReference`; the system cosim checks every STAT read and every
frame against it, including a test with random tiles, objects, scroll and window. The timing
rules the Mooneye tests pin down (when STAT and the STAT interrupt see mode changes, LY reading
the next line early, the LY = LYC flag freezing while the LCD is off, the first line after the
LCD is turned on, when VRAM and OAM are locked against the CPU) are listed there. The PPU locks
the CPU out through `readLock`/`writeLock` handles on the VRAM `RamPlugin` (`lockable = true`)
and on `OamDmaPlugin`.

The DMG OAM corruption bug is modelled too: an address in FE00-FEFF on the CPU's address bus, or
stepped by its 16-bit incrementer (INC/DEC rr, PUSH, POP, LD A,(HL±)), while the PPU scans OAM
rewrites the row being scanned from the rows before it (formulas from Pan Docs). `IsaPlugin` tags
every M-cycle that steps a pair with the `Idu.PAIR` key, derived from the microcode's assigns, and
exports the IDU's address; `OamDmaPlugin` holds OAM in registers so one M-cycle can rewrite three
rows. `Sm83Bus.idu` marks the same M-cycles in the reference model.

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
src/main/scala/retro/r2a03/  Ricoh 2A03 CPU plugins and reference model
src/main/scala/retro/nes/    NES plugins (PPU, APU and DMA, cartridge), the Nes composition, system model
src/main/scala/retro/demo/   toy pipeline exercising the CtrlLink layer
src/test/scala/retro/toy/    toy CPU testing sequencer, pairs, flags, branches, stalls, DMA
src/test/scala/retro/sm83/   SM83 cosimulation (hand-written and random code)
src/test/scala/retro/gb/     Game Boy system cosimulation, known-answer tests of the reference model
src/test/scala/retro/r2a03/  2A03 single-step tests, nestest, cosim, interrupt tests
src/test/scala/retro/nes/    NES test ROM runners (model and RTL) and system cosimulation
docs/EXTENDING.md            adding an instruction, a plugin or a CPU
```

## Roadmap

Done: scaffold, framework layer, the SM83 instruction set with interrupts, extension guide, CPU
and SoC unified in one plugin host, Game Boy SoC (RAMs, IF/IE, timer, serial, PPU, joypad, OAM
DMA, cartridge with MBC1/MBC5, APU) with system cosim.

Validated against hardware test ROMs: Blargg's CPU, timing and HALT tests and Mooneye's CPU,
interrupt, timer, serial and OAM DMA acceptance tests, Mooneye's PPU tests but one, Blargg's OAM
corruption bug tests, dmg-acid2 pixel for pixel, and 2327 ROMs of the c-sp collection, 15 of
Mealybug's 24 DMG tests among them (see *Hardware test ROMs*).

NES: the 2A03 CPU (SingleStepTests nes6502, nestest), a whole-system reference model passing
138 of 144 blargg CPU/PPU/APU/MMC3 test ROMs, with sound checked against hardware recordings, and
the RTL NES (PPU, APU with sound, DMA unit, NROM/MMC1/CNROM/MMC3) cosimulated against it on those
ROMs, audio included.

Next: MBC2, MBC3 and STOP on the Game Boy; on the NES, more mappers.
