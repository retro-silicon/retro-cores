package retro.gb

import retro.core._
import retro.sm83._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

import scala.collection.mutable.ArrayBuffer

/** Game Boy (DMG) constants shared by the RTL plugins and `GbReference`. */
object Gb {
  object Irq {
    val VBLANK = IrqLine("VBlank", 0)
    val STAT = IrqLine("STAT", 1)
    val TIMER = IrqLine("Timer", 2)
    val SERIAL = IrqLine("Serial", 3)
    val JOYPAD = IrqLine("Joypad", 4)
  }

  // I/O register addresses.
  val P1 = 0xFF00; val SB = 0xFF01; val SC = 0xFF02
  val DIV = 0xFF04; val TIMA = 0xFF05; val TMA = 0xFF06; val TAC = 0xFF07
  val IF = 0xFF0F; val IE = 0xFFFF
  val LCDC = 0xFF40; val STAT = 0xFF41; val LY = 0xFF44; val LYC = 0xFF45
  /** LCD registers that are plain storage until there is a PPU to read them. */
  val lcdPlain = Seq(0xFF42, 0xFF43, 0xFF47, 0xFF48, 0xFF49, 0xFF4A, 0xFF4B) // SCY SCX BGP OBP0 OBP1 WY WX

  /** M-cycles per scanline, visible lines, lines per frame; OAM scan and pixel transfer ends. */
  val lineCycles = 114; val visibleLines = 144; val frameLines = 154
  val oamEnd = 20; val transferEnd = 63
  /** M-cycles for an internally clocked serial byte: 8 bits at 8192 Hz. */
  val serialCycles = 1024

  /** State the DMG boot ROM leaves behind when it jumps to the cartridge at 0x0100. The timer's
    * internal counter is the value that makes DIV read 0xAB there. */
  object PostBoot {
    import Sm83Regs._
    val regs: Seq[(Register, BigInt)] = Seq(A -> 0x01, F -> 0xB0, B -> 0x00, C -> 0x13, D -> 0x00, E -> 0xD8,
      H -> 0x01, L -> 0x4D, SPH -> 0xFF, SPL -> 0xFE, PC -> 0x0100)
    val counter = 0xABCC
    val lcdc = 0x91
  }

  def timerTap(counter: Int, tac: Int): Boolean =
    (tac & 4) != 0 && ((counter >> Seq(9, 3, 5, 7)(tac & 3)) & 1) == 1
}

/** IF (0xFF0F) and IE (0xFFFF). Peripherals raise requests through `IrqService`; the CPU's
  * `Sm83.InterruptPlugin` finds this plugin as its `InterruptController`. In a cycle where a
  * request, a write to IF and the CPU's acknowledge coincide, the write lands first, then the
  * request, and the acknowledge clears last. */
class InterruptControllerPlugin extends FiberPlugin with IrqService with Sm83.InterruptController {
  val irqLock = Retainer()
  private val lines = ArrayBuffer[(IrqLine, Bool)]()
  def request(line: IrqLine): Bool = {
    val b = Bool().setCompositeName(this, s"irq_${line.name}")
    lines += line -> b
    b
  }

  val early = during setup new Area {
    val requests = Bits(5 bits)
    val ack = Flow(UInt(3 bits))
  }
  def cpuRequests = early.requests
  def cpuAck = early.ack

  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val ifSlave = bus.newSlave(SingleMapping(Gb.IF))
    val ieSlave = bus.newSlave(SingleMapping(Gb.IE))
    retainer.release()

    val flags = Reg(Bits(5 bits)).init(0).simPublic()
    val enable = Reg(Bits(8 bits)).init(0).simPublic()
    new RetroBusSlaveFactory(ifSlave, idleBits = 0xFF).read(flags, Gb.IF)
    new RetroBusSlaveFactory(ieSlave).readAndWrite(enable, Gb.IE)

    irqLock.await()
    val raised = (0 until 5).map(i => lines.filter(_._1.index == i).map(_._2).foldLeft(False: Bool)(_ || _)).asBits()
    val written = ifSlave.writeFire ? ifSlave.cmd.data(4 downto 0).asBits | flags
    val acked = early.ack.valid ? UIntToOh(early.ack.payload, 5) | B(0, 5 bits)
    flags := (written | raised) & ~acked
    early.requests := flags & enable(4 downto 0)
  }
}

/** Plugins that need a bus slave and maybe interrupt lines, wired the same way. */
abstract class GbPeripheral(mapping: AddressMapping, irqs: IrqLine*) extends FiberPlugin {
  def body(slave: BusSlave, irq: Seq[Bool]): Area

  val logic = during setup new Area {
    val bus = host[BusService]
    val ctrl = host[IrqService]
    val retainers = Seq(retains(bus.busLock), retains(ctrl.irqLock))
    awaitBuild()
    val slave = bus.newSlave(mapping)
    val lines = irqs.map(ctrl.request)
    retainers.foreach(_.release())
    val impl = body(slave, lines)
  }
}

/** DIV, TIMA, TMA, TAC. A 16-bit counter advances by 4 each M-cycle (one per T-cycle); DIV is
  * its top byte, and TIMA counts falling edges of the counter bit TAC selects, gated by the TAC
  * enable, so writing DIV or TAC can itself tick TIMA, as on the DMG.
  *
  * On overflow TIMA reads 0x00 for one M-cycle (A), then loads TMA and requests the interrupt.
  * Writing TIMA during A cancels both. During the next cycle (B) TIMA writes are ignored and TMA
  * writes also land in TIMA. */
class TimerPlugin(resetCounter: Int = 0) extends GbPeripheral(SizeMapping(Gb.DIV, 4), Gb.Irq.TIMER) {
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    val counter = Reg(UInt(16 bits)).init(resetCounter).simPublic()
    val tima = Reg(UInt(8 bits)).init(0).simPublic()
    val tma = Reg(UInt(8 bits)).init(0).simPublic()
    val tac = Reg(UInt(3 bits)).init(0).simPublic()
    val overflowed = RegInit(False)
    val reloading = RegInit(False)

    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.read(counter(15 downto 8), Gb.DIV)
    regs.read(tima, Gb.TIMA)
    regs.read(tma, Gb.TMA)
    regs.read(tac, Gb.TAC)

    def writes(a: Int) = slave.writeFire && slave.cmd.address === a
    val data = slave.cmd.data
    val counterNext = writes(Gb.DIV) ? U(0, 16 bits) | counter + 4
    val tacNext = writes(Gb.TAC) ? data(2 downto 0) | tac
    val tmaNext = writes(Gb.TMA) ? data | tma
    def tap(c: UInt, t: UInt) = t(2) && t(1 downto 0).muxListDc(Seq(0 -> c(9), 1 -> c(3), 2 -> c(5), 3 -> c(7)))
    val fall = tap(counter, tac) && !tap(counterNext, tacNext)

    counter := counterNext
    tac := tacNext
    tma := tmaNext
    overflowed := False
    reloading := False
    irq(0) := False
    def increment() = { tima := tima + 1; when(tima === 0xFF) { overflowed := True } }
    when(overflowed && !writes(Gb.TIMA)) {
      tima := tmaNext
      irq(0) := True
      reloading := True
    } elsewhen (reloading) {
      when(writes(Gb.TMA)) { tima := data } elsewhen (fall) { increment() }
    } elsewhen (writes(Gb.TIMA)) {
      tima := data
    } elsewhen (fall) {
      increment()
    }
  }
}

/** SB and SC. A transfer with the internal clock takes `Gb.serialCycles` M-cycles; with nothing
  * on the link the byte shifted in is 0xFF. `tx` is a pin that pulses with each byte sent, which
  * is how test ROMs report results. */
class SerialPlugin extends GbPeripheral(SizeMapping(Gb.SB, 2), Gb.Irq.SERIAL) {
  var tx: Flow[UInt] = null
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    tx = master(Flow(UInt(8 bits))).setName("serial_tx")
    val sb = Reg(UInt(8 bits)) init 0
    val active = RegInit(False)
    val internalClock = RegInit(False)
    val remaining = Reg(UInt(log2Up(Gb.serialCycles + 1) bits)) init 0

    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.read(sb, Gb.SB)
    regs.read(internalClock, Gb.SC, 0)
    regs.read(active, Gb.SC, 7)

    def writes(a: Int) = slave.writeFire && slave.cmd.address === a
    val data = slave.cmd.data
    tx.valid := False
    irq(0) := False
    tx.payload := sb
    when(writes(Gb.SC)) {
      active := data(7)
      internalClock := data(0)
      remaining := 0
      when(data(7) && data(0)) { remaining := Gb.serialCycles; tx.valid := True }
    } elsewhen (remaining =/= 0) {
      remaining := remaining - 1
      when(remaining === 1) { sb := 0xFF; active := False; irq(0) := True }
    }
    when(writes(Gb.SB)) { sb := data }
  }
}

/** LCD timing without pixels: LCDC, STAT, LY, LYC and their interrupts, with fixed mode lengths
  * (OAM scan 20 M-cycles, transfer 43, HBlank 51). Enough for software that waits on LY or
  * VBlank. The scroll, palette and window registers are plain storage. */
class LcdTimingPlugin(resetLcdc: Int = 0) extends GbPeripheral(SizeMapping(0xFF40, 0x0C), Gb.Irq.VBLANK, Gb.Irq.STAT) {
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    val lcdc = Reg(UInt(8 bits)).init(resetLcdc).simPublic()
    val statSelect = Reg(UInt(4 bits)) init 0
    val lyc = Reg(UInt(8 bits)) init 0
    val x = Reg(UInt(7 bits)).init(0).simPublic()
    val ly = Reg(UInt(8 bits)).init(0).simPublic()
    val statLine = RegInit(False)

    val on = lcdc(7)
    val mode = UInt(2 bits)
    mode := 0
    when(on) {
      when(ly >= Gb.visibleLines) { mode := 1 } elsewhen (x < Gb.oamEnd) { mode := 2 } elsewhen (x < Gb.transferEnd) { mode := 3 }
    }
    val coincidence = ly === lyc

    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.readAndWrite(lcdc, Gb.LCDC)
    regs.readAndWrite(statSelect, Gb.STAT, 3)
    regs.read(coincidence, Gb.STAT, 2)
    regs.read(mode, Gb.STAT, 0)
    regs.read(ly, Gb.LY)
    regs.readAndWrite(lyc, Gb.LYC)
    for (a <- Gb.lcdPlain) regs.createReadAndWrite(UInt(8 bits), a) init 0

    when(!on) {
      x := 0
      ly := 0
    } elsewhen (x === Gb.lineCycles - 1) {
      x := 0
      ly := (ly === Gb.frameLines - 1) ? U(0, 8 bits) | ly + 1
    } otherwise {
      x := x + 1
    }
    irq(0) := on && ly === Gb.visibleLines - 1 && x === Gb.lineCycles - 1
    val line = on && ((statSelect(3) && coincidence) || (statSelect(2) && mode === 2) ||
      (statSelect(1) && mode === 1) || (statSelect(0) && mode === 0))
    statLine := line
    irq(1) := line && !statLine
  }
}

/** P1. `buttons` pins, high when pressed: bits 0-3 Right Left Up Down, bits 4-7 A B Select Start.
  * Any selected input going from released to pressed requests the joypad interrupt. */
class JoypadPlugin extends GbPeripheral(SingleMapping(Gb.P1), Gb.Irq.JOYPAD) {
  var buttons: Bits = null
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    buttons = in(Bits(8 bits)).setName("joypad_buttons")
    val select = Reg(Bits(2 bits)) init 0
    val pressed = (!select(0) ? buttons(3 downto 0) | B"0000") | (!select(1) ? buttons(7 downto 4) | B"0000")
    val lines = ~pressed
    val previous = RegNext(lines).init(B"1111")
    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.readAndWrite(select, Gb.P1, 4)
    regs.read(lines, Gb.P1, 0)
    irq(0) := (previous & ~lines) =/= 0
  }
}

/** Puts the CPU and timer in the state the DMG boot ROM leaves, so a cartridge starts at 0x0100
  * without a boot ROM image. */
class PostBootPlugin extends FiberPlugin {
  val logic = during setup new Area {
    val rf = host[RegFileService]
    for ((r, v) <- Gb.PostBoot.regs) rf.setReset(r, v)
  }
}

/** The Game Boy: the SM83 plugins plus the SoC plugins, in one host. Whatever no plugin maps
  * (cartridge ROM 0000-7FFF and cartridge RAM A000-BFFF) is the external bus. */
object GameBoy {
  def plugins(withTrace: Boolean = true, postBoot: Boolean = true): Seq[Hostable] = Sm83.plugins(withTrace) ++ Seq(
    new RamPlugin("vram", SizeMapping(0x8000, 0x2000), 0x2000),
    new RamPlugin("wram", SizeMapping(0xC000, 0x3E00), 0x2000), // E000-FDFF echoes C000-DDFF
    new RamPlugin("oam", SizeMapping(0xFE00, 0xA0), 0xA0),
    new RamPlugin("hram", SizeMapping(0xFF80, 0x7F), 0x7F),
    new OpenBusPlugin(SizeMapping(0xFEA0, 0x60), value = 0x00),
    new OpenBusPlugin(SizeMapping(0xFF00, 0x100)),
    new InterruptControllerPlugin,
    new TimerPlugin(if (postBoot) Gb.PostBoot.counter else 0),
    new SerialPlugin,
    new LcdTimingPlugin(if (postBoot) Gb.PostBoot.lcdc else 0),
    new JoypadPlugin
  ) ++ (if (postBoot) Seq(new PostBootPlugin) else Nil)

  def apply(withTrace: Boolean = true, postBoot: Boolean = true): RetroCore =
    RetroCore(plugins(withTrace, postBoot)).setDefinitionName("GameBoy")
}

object GameBoyVerilog extends App {
  SpinalConfig(targetDirectory = "gen").generateVerilog(GameBoy(withTrace = false))
}
