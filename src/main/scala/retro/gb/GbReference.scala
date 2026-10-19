package retro.gb

import retro.sm83._

import scala.collection.mutable.ArrayBuffer

/** Game Boy system reference model for cosimulation: `Sm83Reference` attached to models of the
  * on-chip RAMs, IF/IE, timer, serial port, LCD timing and joypad, written from the Pan Docs
  * descriptions independently of the RTL plugins.
  *
  * Every M-cycle a bus access sees the state at the start of the cycle; the cycle's write and
  * the peripherals' tick then produce the state for the next one, as registers do in the RTL.
  *
  * @param external what the cartridge answers at every address no on-chip block claims; writes
  *                 below 0x8000 are ignored (ROM), others are stored
  * @param postBoot start in the state the DMG boot ROM leaves, as `GameBoy(postBoot = true)` */
class GbReference(val external: Array[Int], postBoot: Boolean = true) extends Sm83Bus {
  /** IE & IF at the start of each M-cycle, for the CPU's interrupt decisions. */
  private val requestLog = ArrayBuffer[Int]()
  val cpu = new Sm83Reference(this, k => requestLog(k.toInt), i => iflag &= ~(1 << i))

  val vram, wram = Array.fill(0x2000)(0)
  val oam = Array.fill(0x100)(0)
  val hram = Array.fill(0x80)(0)
  var ie, iflag = 0
  // Timer
  var counter = if (postBoot) Gb.PostBoot.counter else 0
  var tima, tma, tac = 0
  var overflowed, reloading = false
  // Serial
  var sb, remaining = 0
  var active, internalClock = false
  /** Bytes sent with the internal clock. */
  val serialOut = ArrayBuffer[Int]()
  // LCD
  var lcdc = if (postBoot) Gb.PostBoot.lcdc else 0
  var statSelect, lyc, x, ly = 0
  var statLine = false
  val lcdPlain = collection.mutable.Map[Int, Int]().withDefaultValue(0)
  // Joypad: pressed buttons (bits 0-3 Right Left Up Down, 4-7 A B Select Start) and P1 select.
  var buttons = 0
  /** The joypad inputs during M-cycle `k`, when a cosim drives them. */
  var buttonsAt: Long => Int = _ => buttons
  var select = 0
  private var joypadPrevious = 0xF

  if (postBoot) cpu.load(Gb.PostBoot.regs.map { case (r, v) => r.name -> v.toInt }.toMap)

  def mode = if ((lcdc & 0x80) == 0) 0 else if (ly >= Gb.visibleLines) 1 else if (x < Gb.oamEnd) 2 else if (x < Gb.transferEnd) 3 else 0
  def coincidence = ly == lyc
  def joypadLines = {
    val pressed = (if ((select & 1) == 0) buttons & 0xF else 0) | (if ((select & 2) == 0) buttons >> 4 else 0)
    ~pressed & 0xF
  }

  /** What a read returns at the start of the cycle. */
  def peek(a: Int): Int = a match {
    case _ if a >= 0x8000 && a < 0xA000 => vram(a & 0x1FFF)
    case _ if a >= 0xC000 && a < 0xFE00 => wram(a & 0x1FFF)
    case _ if a >= 0xFE00 && a < 0xFEA0 => oam(a & 0xFF)
    case _ if a >= 0xFEA0 && a < 0xFF00 => 0x00
    case _ if a >= 0xFF80 && a < 0xFFFF => hram(a & 0x7F)
    case Gb.IE => ie
    case Gb.IF => 0xE0 | iflag
    case Gb.P1 => 0xC0 | select << 4 | joypadLines
    case Gb.SB => sb
    case Gb.SC => 0x7E | (if (active) 0x80 else 0) | (if (internalClock) 1 else 0)
    case Gb.DIV => counter >> 8
    case Gb.TIMA => tima
    case Gb.TMA => tma
    case Gb.TAC => 0xF8 | tac
    case Gb.LCDC => lcdc
    case Gb.STAT => 0x80 | statSelect << 3 | (if (coincidence) 4 else 0) | mode
    case Gb.LY => ly
    case Gb.LYC => lyc
    case _ if Gb.lcdPlain.contains(a) => lcdPlain(a)
    case _ if a >= 0xFF00 => 0xFF
    case _ => external(a)
  }

  def read(address: Int): Int = cycle(None, address)
  def write(address: Int, value: Int): Unit = cycle(Some(value), address)
  override def idle(): Unit = cycle(None, -1)

  private def cycle(wdata: Option[Int], a: Int): Int = {
    buttons = buttonsAt(requestLog.size)
    requestLog += ie & iflag & 0x1F
    val v = if (a >= 0) peek(a) else 0
    def writes(addr: Int) = wdata.isDefined && a == addr
    val d = wdata.getOrElse(0)
    var raised = 0

    // ---- Timer -----------------------------------------------------------------------------
    val counterNext = if (writes(Gb.DIV)) 0 else (counter + 4) & 0xFFFF
    val tacNext = if (writes(Gb.TAC)) d & 7 else tac
    val tmaNext = if (writes(Gb.TMA)) d else tma
    val fall = Gb.timerTap(counter, tac) && !Gb.timerTap(counterNext, tacNext)
    var timaNext = tima
    var overflowedNext, reloadingNext = false
    def increment() = { timaNext = (tima + 1) & 0xFF; overflowedNext = tima == 0xFF }
    if (overflowed && !writes(Gb.TIMA)) { timaNext = tmaNext; raised |= 1 << Gb.Irq.TIMER.index; reloadingNext = true }
    else if (reloading) { if (writes(Gb.TMA)) timaNext = d else if (fall) increment() }
    else if (writes(Gb.TIMA)) timaNext = d
    else if (fall) increment()

    // ---- Serial ----------------------------------------------------------------------------
    var sbNext = sb
    if (writes(Gb.SC)) {
      active = (d & 0x80) != 0
      internalClock = (d & 1) != 0
      remaining = 0
      if (active && internalClock) { remaining = Gb.serialCycles; serialOut += sb }
    } else if (remaining != 0) {
      remaining -= 1
      if (remaining == 0) { sbNext = 0xFF; active = false; raised |= 1 << Gb.Irq.SERIAL.index }
    }
    if (writes(Gb.SB)) sbNext = d

    // ---- LCD timing ------------------------------------------------------------------------
    val on = (lcdc & 0x80) != 0
    if (on && ly == Gb.visibleLines - 1 && x == Gb.lineCycles - 1) raised |= 1 << Gb.Irq.VBLANK.index
    val line = on && (((statSelect & 8) != 0 && coincidence) || ((statSelect & 4) != 0 && mode == 2) ||
      ((statSelect & 2) != 0 && mode == 1) || ((statSelect & 1) != 0 && mode == 0))
    if (line && !statLine) raised |= 1 << Gb.Irq.STAT.index
    statLine = line
    if (!on) { x = 0; ly = 0 }
    else if (x == Gb.lineCycles - 1) { x = 0; ly = if (ly == Gb.frameLines - 1) 0 else ly + 1 }
    else x += 1

    // ---- Joypad ----------------------------------------------------------------------------
    val lines = joypadLines
    if ((joypadPrevious & ~lines) != 0) raised |= 1 << Gb.Irq.JOYPAD.index
    joypadPrevious = lines

    // ---- Register writes -------------------------------------------------------------------
    for (_ <- wdata) a match {
      case _ if a < 0x8000 =>
      case _ if a < 0xA000 => vram(a & 0x1FFF) = d
      case _ if a < 0xC000 => external(a) = d
      case _ if a < 0xFE00 => wram(a & 0x1FFF) = d
      case _ if a < 0xFEA0 => oam(a & 0xFF) = d
      case _ if a >= 0xFF80 && a < 0xFFFF => hram(a & 0x7F) = d
      case Gb.IE => ie = d
      case Gb.P1 => select = (d >> 4) & 3
      case Gb.LCDC => lcdc = d
      case Gb.STAT => statSelect = (d >> 3) & 0xF
      case Gb.LYC => lyc = d
      case _ if Gb.lcdPlain.contains(a) => lcdPlain(a) = d
      case _ =>
    }
    counter = counterNext; tac = tacNext; tma = tmaNext; tima = timaNext
    overflowed = overflowedNext; reloading = reloadingNext
    sb = sbNext
    iflag = ((if (writes(Gb.IF)) d & 0x1F else iflag) | raised) & 0x1F
    v
  }
}
