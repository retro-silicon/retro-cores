package retro.nes

import retro.core._
import retro.r2a03._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

import scala.collection.mutable.ArrayBuffer

/** NES (NTSC) constants shared by the RTL plugins and `NesReference`.
  *
  * Timing convention, the same as the model's: each CPU cycle the PPU runs two dots, the APU
  * ticks, the bus access happens (seeing that state, writes landing on top), the PPU runs its
  * third dot, and the CPU samples NMI and IRQ as they are at the end of the cycle. The RTL runs
  * on a clock three times the CPU's, one PPU dot per clock; the CPU and the rest tick on the
  * third (`CoreClocks`). */
object Nes {
  val OAMDMA = 0x4014; val APU_STATUS = 0x4015; val JOY1 = 0x4016; val FRAME_COUNTER = 0x4017
  /** The CPU's interrupt lines (`NesInterruptsPlugin`): the PPU drives NMI, the APU and the
    * mapper IRQ. */
  object Irq {
    val NMI = IrqLine("NMI", 0)
    val IRQ = IrqLine("IRQ", 1)
  }
  /** APU length counter loads, indexed by the top 5 bits of the 4003/4007/400B/400F write. */
  val lengthTable = Seq(10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
                        12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30)

  /** The 2A03 plugins plus the NES ones, in one host. The open bus claims every address no
    * other slave does, so the CPU has no external bus port. `romDecoder`: see `SequencerPlugin`
    * (the simulations use it). */
  def plugins(cart: NesCartSpec, withTrace: Boolean = true, romDecoder: Boolean = false, image: Option[NesCartridge] = None,
              timing: NesTiming = NesTiming.ntsc): Seq[Hostable] =
    Ricoh2A03.plugins(withTrace, external = false, romDecoder = romDecoder) ++ Seq(
    new NesInterruptsPlugin,
    new OpenBusPlugin(SizeMapping(0x0000, 0x10000), latched = true),
    new RamPlugin("ram", SizeMapping(0x0000, 0x2000), 0x800, midCycleRead = true), // 0800-1FFF mirror it
    new NesPpuPlugin(timing),
    new NesApuPlugin(timing),
    new NesJoypadPlugin,
    new NesCartridgePlugin(cart, image))

  def apply(cart: NesCartSpec, withTrace: Boolean = true, romDecoder: Boolean = false, image: Option[NesCartridge] = None,
            timing: NesTiming = NesTiming.ntsc): RetroCore =
    RetroCore(plugins(cart, withTrace, romDecoder, image, timing), cycleClocks = timing.dotsPerCycle).setDefinitionName("Nes")
}

/** What differs between the NTSC console (2A03, 2C02) and the PAL one (2A07, 2C07), as far as
  * modelled: the PPU's 312 lines with no skipped dot, 3.2 PPU dots per CPU cycle (four dots
  * every fifth cycle, after the access), the frame counter's steps (blargg's pal_apu_tests),
  * and the noise and DMC period tables. */
case class NesTiming(pal: Boolean) {
  /** The frame counter's four steps from its reset; the 4-step IRQ flag is set from the cycle
    * before the fourth to the one after, when the counter wraps. */
  val steps = if (pal) Seq(8313, 16627, 24939, 33253) else Seq(7457, 14913, 22371, 29829)
  /** The 5-step mode's fifth step; it wraps a cycle later. */
  val fifthStep = if (pal) 41565 else 37281
  val noisePeriods = if (pal) Seq(4, 8, 14, 30, 60, 88, 118, 148, 188, 236, 354, 472, 708, 944, 1890, 3778)
                     else Seq(4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068)
  val dmcRates = if (pal) Seq(398, 354, 316, 298, 276, 236, 210, 198, 176, 148, 132, 118, 98, 78, 66, 50)
                 else Seq(428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54)
  /** The pre-render line; on NTSC its last dot is skipped on odd frames while rendering. */
  val preLine = if (pal) 311 else 261
  val oddFrameSkip = !pal
  /** PPU dots in each CPU cycle, repeating. */
  val dotsPerCycle = if (pal) Seq(3, 3, 3, 3, 4) else Seq(3)
}
object NesTiming {
  val ntsc = NesTiming(pal = false)
  val pal = NesTiming(pal = true)
}

/** The CPU's NMI and IRQ inputs (`Nes.Irq`): peripherals drive them through `IrqService`, as
  * levels. `Ricoh2A03.InterruptPlugin` finds this plugin and wires its lines to them. */
class NesInterruptsPlugin extends FiberPlugin with IrqService with Ricoh2A03.InterruptLines {
  val irqLock = Retainer()
  private val requests = ArrayBuffer[(IrqLine, Bool)]()
  def request(line: IrqLine): Bool = {
    val b = Bool()
    requests += line -> b
    b
  }
  val early = during setup new Area { val nmi, irq, reset = Bool() }
  def nmi = early.nmi
  def irq = early.irq
  override def reset = Some(early.reset)
  /** High in the opcode fetch that takes a reset button press, for the APU. */
  def resetTaken: Bool = logic.resetTaken
  /** The reset button (`reset_button` pin): a pulse is held as a request until the CPU's next
    * opcode fetch takes it. */
  val logic = during setup new Area {
    val seq = host[SequencerService]
    awaitBuild()
    val button = in(Bool()).setName("reset_button")
    val request = RegInit(False)
    when(button) { request := True }
    val resetTaken = seq.fetchFire && request
    when(resetTaken) { request := False }
    early.reset := request
    irqLock.await()
    def any(line: IrqLine) = requests.filter(_._1 == line).map(_._2).foldLeft(False: Bool)(_ || _)
    early.nmi := any(Nes.Irq.NMI)
    early.irq := any(Nes.Irq.IRQ)
  }
}

/** Controller port 1 (4016): the `buttons` pins (A B Select Start Up Down Left Right in bits 0-7)
  * are registered, so a change shows a cycle later. A write sets the strobe, which while high
  * reloads the shift register; each read returns bit 0 and shifts in a 1. Bits 7-5 read open
  * bus. */
class NesJoypadPlugin extends PeripheralPlugin(SingleMapping(Nes.JOY1)) {
  val pins = during setup new Area { val buttons = in(Bits(8 bits)).setName("joypad1") }
  def buttons = pins.buttons
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    import slave._
    val open = host[OpenBusPlugin].data
    // The pins are registered, as asynchronous inputs should be. It also keeps the bus read data,
    // and so most of the design, off the inputs, which Verilator re-evaluates on every edge.
    val pad = RegNext(buttons).init(0)
    val strobe = RegInit(False)
    val shift = Reg(UInt(8 bits)).init(0)
    val bit = strobe ? pad(0) | shift(0)
    rdata := open(7 downto 5) @@ U(0, 4 bits) @@ bit.asUInt
    when(reads(Nes.JOY1) && !strobe) { shift := U(1, 1 bits) @@ shift(7 downto 1) }
    when(writes(Nes.JOY1)) {
      strobe := cmd.data(0)
      when(cmd.data(0)) { shift := pad.asUInt }
    }
  }
}

/** Synthesis Verilog for an NROM cartridge (32 KB PRG, 8 KB CHR ROM, filled with stand-in random
  * bytes so synthesis keeps them), without trace taps:
  * {{{ sbt "runMain retro.nes.NesVerilog" }}} writes gen/Nes.v. */
object NesVerilog extends App {
  val random = new scala.util.Random(1)
  val cart = NesCartridge(Array.fill(0x8000)(random.nextInt(256)), Array.fill(0x2000)(random.nextInt(256)), chrRam = false, mapper = 0, verticalMirroring = true)
  SpinalConfig(targetDirectory = "gen").generateVerilog(Nes(NesCartSpec.of(cart), withTrace = false, image = Some(cart)))
}
