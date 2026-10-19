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

/** Game Boy (DMG) constants shared by the RTL plugins and `GbReference`.
  *
  * Timing convention for every peripheral: in each M-cycle the peripheral ticks first (its four
  * T-cycles), then the CPU's access sees the ticked state, and a write lands on top of it. In the
  * RTL a register holds the state at the start of the cycle, so reads return the ticked value
  * combinationally and the next state is the ticked one with the write applied. Mooneye's
  * div_timing and Blargg's mem_timing pin this down. */
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
  val LCDC = 0xFF40; val STAT = 0xFF41; val SCY = 0xFF42; val SCX = 0xFF43; val LY = 0xFF44; val LYC = 0xFF45
  val DMA = 0xFF46; val BGP = 0xFF47; val OBP0 = 0xFF48; val OBP1 = 0xFF49; val WY = 0xFF4A; val WX = 0xFF4B

  /** M-cycles per scanline, visible lines, lines per frame; the M-cycle the OAM scan ends. */
  val lineCycles = 114; val visibleLines = 144; val frameLines = 154
  val oamEnd = 20
  /** OAM: 40 objects of 4 bytes, FE00-FE9F. */
  val oamBytes = 160
  /** The timer counter bit whose falling edges clock the serial port (8192 Hz). */
  val serialClockBit = 8

  /** State the DMG boot ROM leaves behind when it jumps to the cartridge at 0x0100. The timer's
    * internal counter is the value that makes DIV read 0xAB there. */
  object PostBoot {
    import Sm83Regs._
    val regs: Seq[(Register, BigInt)] = Seq(A -> 0x01, F -> 0xB0, B -> 0x00, C -> 0x13, D -> 0x00, E -> 0xD8,
      H -> 0x01, L -> 0x4D, SPH -> 0xFF, SPL -> 0xFE, PC -> 0x0100)
    val counter = 0xABC8
    val lcdc = 0x91
  }

  def timerTap(counter: Int, tac: Int): Boolean =
    (tac & 4) != 0 && ((counter >> Seq(9, 3, 5, 7)(tac & 3)) & 1) == 1
}

/** IF (0xFF0F) and IE (0xFFFF). Peripherals raise requests through `IrqService`; the CPU's
  * `Sm83.InterruptPlugin` finds this plugin as its `InterruptController`. Requests raised by
  * this M-cycle's tick are visible in the same cycle, to a read of IF and to the CPU's interrupt
  * check; a write to IF then lands over them, and the CPU's acknowledge clears last. */
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

    irqLock.await()
    val raised = (0 until 5).map(i => lines.filter(_._1.index == i).map(_._2).foldLeft(False: Bool)(_ || _)).asBits()
    val ticked = flags | raised
    new RetroBusSlaveFactory(ifSlave, idleBits = 0xFF).read(ticked, Gb.IF)
    new RetroBusSlaveFactory(ieSlave).readAndWrite(enable, Gb.IE)
    val written = ifSlave.writeFire ? ifSlave.cmd.data(4 downto 0).asBits | ticked
    val acked = early.ack.valid ? UIntToOh(early.ack.payload, 5) | B(0, 5 bits)
    flags := written & ~acked
    early.requests := ticked & enable(4 downto 0)
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
  * Like every Game Boy peripheral here, each M-cycle the timer ticks first and the CPU's access
  * then sees, or writes over, the ticked state (see `Gb`). On overflow TIMA reads 0x00 for one
  * M-cycle (A); the next tick loads TMA and requests the interrupt (B). Writing TIMA during A
  * cancels both. During B a TIMA write is ignored and a TMA write also lands in TIMA. */
class TimerPlugin(resetCounter: Int = 0) extends GbPeripheral(SizeMapping(Gb.DIV, 4), Gb.Irq.TIMER) {
  /** The 16-bit counter, and its value after this M-cycle's tick: other peripherals (serial)
    * divide their clocks from it. */
  val counter, tickedCounter = Handle[UInt]()
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    val counter = Reg(UInt(16 bits)).init(resetCounter).simPublic()
    TimerPlugin.this.counter.load(counter)
    val tima = Reg(UInt(8 bits)).init(0).simPublic()
    val tma = Reg(UInt(8 bits)).init(0).simPublic()
    val tac = Reg(UInt(3 bits)).init(0).simPublic()
    val overflowed = RegInit(False)

    def tap(c: UInt, t: UInt) = t(2) && t(1 downto 0).muxListDc(Seq(0 -> c(9), 1 -> c(3), 2 -> c(5), 3 -> c(7)))
    // The tick: this M-cycle's four T-cycles, before the CPU's access.
    val ticked = new Area {
      val counter = UInt(16 bits)
      val tima = UInt(8 bits)
      val overflowed, reloading = Bool()
    }
    ticked.counter := counter + 4
    tickedCounter.load(ticked.counter)
    ticked.tima := tima
    ticked.overflowed := False
    ticked.reloading := False
    irq(0) := False
    when(overflowed) {
      ticked.tima := tma
      ticked.reloading := True
      irq(0) := True
    } elsewhen (tap(counter, tac) && !tap(ticked.counter, tac)) {
      ticked.tima := tima + 1
      ticked.overflowed := tima === 0xFF
    }

    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.read(ticked.counter(15 downto 8), Gb.DIV)
    regs.read(ticked.tima, Gb.TIMA)
    regs.read(tma, Gb.TMA)
    regs.read(tac, Gb.TAC)

    def writes(a: Int) = slave.writeFire && slave.cmd.address === a
    val data = slave.cmd.data
    val counterNext = writes(Gb.DIV) ? U(0, 16 bits) | ticked.counter
    val tacNext = writes(Gb.TAC) ? data(2 downto 0) | tac
    // Resetting DIV or changing TAC can drop the selected bit: that is a falling edge too.
    val glitch = tap(ticked.counter, tac) && !tap(counterNext, tacNext)

    counter := counterNext
    tac := tacNext
    tima := ticked.tima
    overflowed := ticked.overflowed
    when(writes(Gb.TIMA) && !ticked.reloading) {
      tima := data
      overflowed := False
    }
    when(writes(Gb.TMA)) {
      tma := data
      when(ticked.reloading) { tima := data }
    }
    when(glitch) {
      tima := ticked.tima + 1
      overflowed := ticked.tima === 0xFF
    }
  }
}

/** SB and SC. With the internal clock a transfer shifts one bit out of SB (and a 1 in, nothing
  * being on the link) on each falling edge of bit 8 of the timer's counter (8192 Hz), so eight
  * bits take 1024 M-cycles but start aligned to that clock, not to the SC write. `tx` is a pin
  * that pulses with each byte sent, which is how test ROMs report results. */
class SerialPlugin extends GbPeripheral(SizeMapping(Gb.SB, 2), Gb.Irq.SERIAL) {
  var tx: Flow[UInt] = null
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    tx = master(Flow(UInt(8 bits))).setName("serial_tx")
    val timer = host[TimerPlugin]
    val sb = Reg(UInt(8 bits)) init 0
    val active = RegInit(False)
    val internalClock = RegInit(False)
    val bits = Reg(UInt(4 bits)) init 0

    // The tick.
    val shift = active && internalClock && timer.counter.get(Gb.serialClockBit) && !timer.tickedCounter.get(Gb.serialClockBit)
    val done = shift && bits === 1
    val sbTicked = shift ? (sb(6 downto 0) @@ U"1") | sb
    val activeTicked = active && !done
    irq(0) := done

    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.read(sbTicked, Gb.SB)
    regs.read(internalClock, Gb.SC, 0)
    regs.read(activeTicked, Gb.SC, 7)

    def writes(a: Int) = slave.writeFire && slave.cmd.address === a
    val data = slave.cmd.data
    sb := sbTicked
    active := activeTicked
    when(shift) { bits := bits - 1 }
    tx.valid := False
    tx.payload := sbTicked
    when(writes(Gb.SC)) {
      active := data(7)
      internalClock := data(0)
      bits := 8
      tx.valid := data(7) && data(0)
    }
    when(writes(Gb.SB)) { sb := data }
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
  * (cartridge ROM 0000-7FFF and cartridge RAM A000-BFFF) is the external bus, unless a
  * `cartridge` is given, in which case a `CartridgePlugin` answers there. */
object GameBoy {
  def plugins(withTrace: Boolean = true, postBoot: Boolean = true, cartridge: Option[CartridgeSpec] = None): Seq[Hostable] = Sm83.plugins(withTrace) ++ Seq(
    new RamPlugin("vram", SizeMapping(0x8000, 0x2000), 0x2000, lockable = true), // locked by the PPU
    new RamPlugin("wram", SizeMapping(0xC000, 0x3E00), 0x2000), // E000-FDFF echoes C000-DDFF
    new OamDmaPlugin,
    new RamPlugin("hram", SizeMapping(0xFF80, 0x7F), 0x7F),
    new OpenBusPlugin(SizeMapping(0xFEA0, 0x60), value = 0x00),
    new OpenBusPlugin(SizeMapping(0xFF00, 0x100)),
    new InterruptControllerPlugin,
    new TimerPlugin(if (postBoot) Gb.PostBoot.counter else 0),
    new SerialPlugin,
    new PpuPlugin(if (postBoot) Gb.PostBoot.lcdc else 0),
    new JoypadPlugin
  ) ++ (if (postBoot) Seq(new PostBootPlugin) else Nil) ++ cartridge.map(new CartridgePlugin(_))

  def apply(withTrace: Boolean = true, postBoot: Boolean = true, cartridge: Option[CartridgeSpec] = None): RetroCore =
    RetroCore(plugins(withTrace, postBoot, cartridge)).setDefinitionName("GameBoy")
}

object GameBoyVerilog extends App {
  SpinalConfig(targetDirectory = "gen").generateVerilog(GameBoy(withTrace = false))
}
