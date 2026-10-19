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
  // The CPU only looks a few M-cycles back, so a ring keeps long runs in bounded memory.
  private var dmaByte, dmaRaw = 0
  private def isVram(a: Int) = a >= 0x8000 && a < 0xA000
  private val requestLog = Array.fill(64)(0)
  private var cycles = 0L
  val cpu = new Sm83Reference(this, k => { require(cycles - k <= 64); requestLog((k & 63).toInt) }, i => iflag &= ~(1 << i))

  val vram = if (postBoot) Gb.PostBoot.vram(external) else Array.fill(0x2000)(0)
  val wram = Array.fill(0x2000)(0)
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
    case _ if a >= 0x8000 && a < 0xA000 => if (ppu.vramReadLocked) 0xFF else vram(a & 0x1FFF)
    case _ if a >= 0xC000 && a < 0xFE00 => wram(a & 0x1FFF)
    case _ if a >= 0xFE00 && a < 0xFEA0 => if (dmaCopying || ppu.oamReadLocked) 0xFF else oam(a & 0xFF)
    case _ if a >= 0xFEA0 && a < 0xFF00 => if (dmaCopying) 0xFF else 0x00
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

  /** Called with every write to the PPU's registers (address, value), for tracing. */
  var onPpuWrite: (Int, Int) => Unit = null
  /** Called with every CPU access to I/O registers (write?, address, value), for tracing. */
  var onIoAccess: (Boolean, Int, Int) => Unit = null
  /** Called when the STAT interrupt is requested, for tracing. */
  var onStatIrq: () => Unit = null

  private var iduAddress = -1
  override def idu(address: Int): Unit = iduAddress = address & 0xFFFF

  /** The DMG OAM corruption bug (Pan Docs, "OAM Corruption Bug"): an address in FE00-FEFF on the
    * address bus while the PPU scans OAM rewrites the row the scan reads, from the row before it.
    * 16-bit words of 8-byte rows: `w(r, i)` is word `i` of row `r`. */
  private def oamBug(row: Int, read: Boolean, increase: Boolean): Unit = {
    def w(r: Int, i: Int) = oam(8 * r + 2 * i) | oam(8 * r + 2 * i + 1) << 8
    def setW(r: Int, i: Int, v: Int) = { oam(8 * r + 2 * i) = v & 0xFF; oam(8 * r + 2 * i + 1) = (v >> 8) & 0xFF }
    def copyRow(from: Int, to: Int) = for (i <- 0 until 8) oam(8 * to + i) = oam(8 * from + i)
    def copyTail(from: Int, to: Int) = for (i <- 2 until 8) oam(8 * to + i) = oam(8 * from + i)
    if (row == 0) return
    if (read && increase && row >= 4 && row < 19) {
      val (a, b, c, d) = (w(row - 2, 0), w(row - 1, 0), w(row, 0), w(row - 1, 2))
      setW(row - 1, 0, (b & (a | c | d)) | (a & c & d))
      copyRow(row - 1, row - 2); copyRow(row - 1, row)
    }
    val (a, b, c) = (w(row, 0), w(row - 1, 0), w(row - 1, 2))
    setW(row, 0, if (read) b | (a & c) else ((a ^ c) & (b ^ c)) ^ c)
    copyTail(row - 1, row)
  }

  def read(address: Int): Int = cycle(None, address)
  def write(address: Int, value: Int): Unit = cycle(Some(value), address)
  override def idle(): Unit = cycle(None, -1)

  private def cycle(wdata: Option[Int], a: Int): Int = {
    buttons = buttonsAt(cycles)
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
    // PPU. A write to its registers lands during its dots (`PpuReference.writeDots`).
    if (a >= Gb.LCDC && a <= Gb.WX && a != Gb.LY && a != Gb.DMA) for (v <- wdata) ppu.schedule(a, v)
    val (vblank, stat) = ppu.tick()
    if (vblank) iflag |= 1 << Gb.Irq.VBLANK.index
    if (stat) { iflag |= 1 << Gb.Irq.STAT.index; if (onStatIrq != null) onStatIrq() }
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
      dmaByte = oam(at); dmaRaw = from << 8 | at
      dmaSource = from; dmaIndex = at + 1; dmaActive = at != 159
    }
    if (dmaStartIn != 0) dmaStartIn -= 1
    // Joypad
    val lines = joypadLines
    if ((joypadPrevious & ~lines) != 0) iflag |= 1 << Gb.Irq.JOYPAD.index
    joypadPrevious = lines

    // ---- Access ----------------------------------------------------------------------------
    // Bits 8-12: requests a halted CPU sees an M-cycle late; bits 16-20: ones a running CPU does.
    requestLog((cycles & 63).toInt) = ie & iflag & 0x1F | (if (ppu.statLate) (1 << Gb.Irq.STAT.index) << 8 else 0) |
      (if (ppu.statRunLate) (1 << Gb.Irq.STAT.index) << 16 else 0)
    cycles += 1
    val idu = iduAddress
    iduAddress = -1
    val bugAddress = if (a >= 0) a else idu
    if (bugAddress >= 0xFE00 && bugAddress < 0xFF00 && ppu.oamBugRow >= 0 && !dmaCopying)
      oamBug(ppu.oamBugRow, read = a >= 0 && wdata.isEmpty, increase = idu >= 0)
    // OAM DMA bus conflicts (SameBoy, DMG): while DMA copies, the CPU's accesses to the bus it
    // reads from (VRAM, or the external bus for everything else below FE00) meet DMA's access:
    // a read returns the byte DMA reads, and a write replaces the byte being copied into OAM, or
    // for a source in A000-FDFF ANDs into it (Gambatte's oamdma tests). Neither reaches the bus.
    val conflict = dmaCopying && a >= 0 && a < 0xFE00 && isVram(a) == isVram(dmaRaw)
    val v = if (conflict) dmaByte else if (a >= 0) peek(a) else 0
    if (conflict) for (_ <- wdata)
      oam(dmaRaw & 0xFF) = if (dmaRaw >= 0xA000 && !isVram(dmaRaw)) dmaByte & d else d
    if (onIoAccess != null && a >= 0xFF00 && (a < 0xFF80 || a == 0xFFFF)) onIoAccess(wdata.isDefined, a, wdata.getOrElse(v))
    for (_ <- wdata if !conflict) {
      val (counterTicked, tacTicked) = (counter, tac)
      a match {
        case _ if a < 0x8000 =>
        case _ if a < 0xA000 => if (!ppu.vramWriteLocked) vram(a & 0x1FFF) = d
        case _ if a < 0xC000 => external(a) = d
        case _ if a < 0xFE00 => wram(a & 0x1FFF) = d
        case _ if a < 0xFEA0 => if (!dmaCopying && !ppu.oamWriteLocked) oam(a & 0xFF) = d
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
        case _ if a >= Gb.LCDC && a <= Gb.WX => if (onPpuWrite != null) onPpuWrite(a, d) // landed in the tick
        case _ =>
      }
      // Resetting DIV or changing TAC can drop the selected bit: that is a falling edge too.
      if (Gb.timerTap(counterTicked, tacTicked) && !Gb.timerTap(counter, tac)) { overflowed = tima == 0xFF; tima = (tima + 1) & 0xFF }
    }
    v
  }
}
