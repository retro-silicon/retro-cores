package retro.gb

import retro.sm83._

import scala.collection.mutable.ArrayBuffer

/** Game Boy system reference model for cosimulation: `Sm83Reference` attached to models of the
  * on-chip RAMs, IF/IE, timer, serial port, PPU (`PpuReference`) and joypad, written from the Pan Docs
  * descriptions independently of the RTL plugins.
  *
  * Every M-cycle the peripherals tick first, then the bus access sees the ticked state and a
  * write lands on top of it (the convention described at `Gb`).
  *
  * @param external what the cartridge answers at every address no on-chip block claims; writes
  *                 below 0x8000 are ignored (ROM), others are stored
  * @param postBoot start in the state the DMG boot ROM leaves, as `GameBoy(postBoot = true)`
  * @param externalDma OAM DMA can read `external` (the RTL has a `CartridgePlugin`); otherwise
  *                    DMA sources outside VRAM and WRAM read FF, as with the external bus */
class GbReference(val external: Array[Int], postBoot: Boolean = true, externalDma: Boolean = false) extends Sm83Bus {
  /** IE & IF after each M-cycle's tick, for the CPU's interrupt decisions. */
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
  var sb, bits = 0
  var active, internalClock = false
  /** Bytes sent with the internal clock. */
  val serialOut = ArrayBuffer[Int]()
  val ppu = new PpuReference(vram, oam, postBoot)
  // OAM DMA
  var dmaRegister = 0xFF
  var dmaStartIn, dmaSource, dmaIndex = 0
  var dmaActive, dmaCopying = false
  // Joypad: pressed buttons (bits 0-3 Right Left Up Down, 4-7 A B Select Start) and P1 select.
  var buttons = 0
  /** The joypad inputs during M-cycle `k`, when a cosim drives them. */
  var buttonsAt: Long => Int = _ => buttons
  var select = 0
  private var joypadPrevious = 0xF

  if (postBoot) cpu.load(Gb.PostBoot.regs.map { case (r, v) => r.name -> v.toInt }.toMap)

  def joypadLines = {
    val pressed = (if ((select & 1) == 0) buttons & 0xF else 0) | (if ((select & 2) == 0) buttons >> 4 else 0)
    ~pressed & 0xF
  }

  /** What a read returns once the cycle's tick is done. */
  def peek(a: Int): Int = a match {
    case _ if a >= 0x8000 && a < 0xA000 => vram(a & 0x1FFF)
    case _ if a >= 0xC000 && a < 0xFE00 => wram(a & 0x1FFF)
    case _ if a >= 0xFE00 && a < 0xFEA0 => if (dmaCopying) 0xFF else oam(a & 0xFF)
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
    case Gb.DMA => dmaRegister
    case _ if a >= Gb.LCDC && a <= Gb.WX => ppu.read(a)
    case _ if a >= 0xFF00 => 0xFF
    case _ => external(a)
  }

  def read(address: Int): Int = cycle(None, address)
  def write(address: Int, value: Int): Unit = cycle(Some(value), address)
  override def idle(): Unit = cycle(None, -1)

  private def cycle(wdata: Option[Int], a: Int): Int = {
    buttons = buttonsAt(requestLog.size)
    def writes(addr: Int) = wdata.isDefined && a == addr
    val d = wdata.getOrElse(0)

    // ---- Tick ------------------------------------------------------------------------------
    // Timer
    val counterBefore = counter
    counter = (counter + 4) & 0xFFFF
    reloading = false
    if (overflowed) { tima = tma; overflowed = false; reloading = true; iflag |= 1 << Gb.Irq.TIMER.index }
    else if (Gb.timerTap(counterBefore, tac) && !Gb.timerTap(counter, tac)) { overflowed = tima == 0xFF; tima = (tima + 1) & 0xFF }
    // Serial
    if (active && internalClock && ((counterBefore >> Gb.serialClockBit) & 1) == 1 && ((counter >> Gb.serialClockBit) & 1) == 0) {
      sb = ((sb << 1) | 1) & 0xFF
      bits -= 1
      if (bits == 0) { active = false; iflag |= 1 << Gb.Irq.SERIAL.index }
    }
    // PPU
    val (vblank, stat) = ppu.tick()
    if (vblank) iflag |= 1 << Gb.Irq.VBLANK.index
    if (stat) iflag |= 1 << Gb.Irq.STAT.index
    // OAM DMA
    dmaCopying = dmaStartIn == 1 || dmaActive
    if (dmaCopying) {
      val from = if (dmaStartIn == 1) dmaRegister else dmaSource
      val at = if (dmaStartIn == 1) 0 else dmaIndex
      val src = if (from >= 0xE0) (from << 8 | at) & ~0x2000 else from << 8 | at
      oam(at) = src match {
        case _ if src >= 0x8000 && src < 0xA000 => vram(src & 0x1FFF)
        case _ if src >= 0xC000 => wram(src & 0x1FFF)
        case _ if externalDma => external(src)
        case _ => 0xFF
      }
      dmaSource = from; dmaIndex = at + 1; dmaActive = at != 159
    }
    if (dmaStartIn != 0) dmaStartIn -= 1
    // Joypad
    val lines = joypadLines
    if ((joypadPrevious & ~lines) != 0) iflag |= 1 << Gb.Irq.JOYPAD.index
    joypadPrevious = lines

    // ---- Access ----------------------------------------------------------------------------
    requestLog += ie & iflag & 0x1F
    val v = if (a >= 0) peek(a) else 0
    for (_ <- wdata) {
      val (counterTicked, tacTicked) = (counter, tac)
      a match {
        case _ if a < 0x8000 =>
        case _ if a < 0xA000 => vram(a & 0x1FFF) = d
        case _ if a < 0xC000 => external(a) = d
        case _ if a < 0xFE00 => wram(a & 0x1FFF) = d
        case _ if a < 0xFEA0 => if (!dmaCopying) oam(a & 0xFF) = d
        case _ if a >= 0xFF80 && a < 0xFFFF => hram(a & 0x7F) = d
        case Gb.IE => ie = d
        case Gb.IF => iflag = d & 0x1F
        case Gb.P1 => select = (d >> 4) & 3
        case Gb.SB => sb = d
        case Gb.SC =>
          active = (d & 0x80) != 0
          internalClock = (d & 1) != 0
          bits = 8
          if (active && internalClock) serialOut += sb
        case Gb.DIV => counter = 0
        case Gb.TIMA => if (!reloading) { tima = d; overflowed = false }
        case Gb.TMA => tma = d; if (reloading) tima = d
        case Gb.TAC => tac = d & 7
        case Gb.DMA => dmaRegister = d; dmaStartIn = 2
        case _ if a >= Gb.LCDC && a <= Gb.WX => ppu.write(a, d)
        case _ =>
      }
      // Resetting DIV or changing TAC can drop the selected bit: that is a falling edge too.
      if (Gb.timerTap(counterTicked, tacTicked) && !Gb.timerTap(counter, tac)) { overflowed = tima == 0xFF; tima = (tima + 1) & 0xFF }
    }
    v
  }
}
