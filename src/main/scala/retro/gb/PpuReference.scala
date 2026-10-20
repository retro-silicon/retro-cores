package retro.gb

/** Dot-level model of the DMG PPU for `GbReference`; `PpuPlugin` is its RTL. Each M-cycle is four
  * dots; `tick` runs them.
  *
  * A line is 456 dots: OAM scan (80 dots, two OAM entries per M-cycle, up to ten objects), pixel
  * transfer, then HBlank. Lines 144-153 are VBlank. The pixel transfer is a port of SameBoy's DMG
  * mode 3 (display.c), which Mealybug's mid-scanline register write tests are checked against:
  * a background FIFO filled by a fetcher (tile number, low byte, high byte, two dots each, then a
  * push once the FIFO is empty), an object FIFO, and a state machine (`sbStep`) that, per dot,
  * checks for the window, fetches the objects at the current X, then shifts out a pixel and steps
  * the fetcher. The first pixels are dropped left of the screen, SCX mod 8 more as the low bits
  * of SCX match, and objects with X < 8 are fetched there.
  *
  * Timing seen by the CPU (Mooneye's PPU tests, Mealybug, SameBoy):
  *  - STAT reads the mode after the M-cycle's third dot, and the STAT interrupt sees the
  *    transfer state after its second dot (before the dots in the transfer's first M-cycle);
  *    mode 3 ends for both 3 dots before the transfer's last pixel (`ended`);
  *  - a write to a PPU register lands during the dots (`writeDot`): SCX before the first,
  *    SCY, LCDC and the palettes before the second (the palettes show old | new and LCDC
  *    old | new BG_EN for one dot first), WY and WX before the third; STAT and LYC after them.
  *    Writes to SCY, SCX, the palettes, WX and LCDC (keeping the LCD on or off and the object
  *    size) land in the next M-cycle's dots (`lateWrite`);
  *  - the mode 2 interrupt of lines 1 to 143 rises in the line before's last M-cycle, with LY;
  *    line 0's an M-cycle later (Mealybug's line_0_fix, Gambatte's m2int tests);
  *  - a halted CPU sees a STAT interrupt raised after the M-cycle's first dot an M-cycle late
  *    (`statLate`);
  *  - LY reads the next line in the last M-cycle of a line, and 0 from the second M-cycle of
  *    line 153, where LY = LYC also drops for one M-cycle;
  *  - the LY = LYC flag is only updated while the LCD is on, and stays on the STAT interrupt line
  *    while it is off (stat_lyc_onoff); it drops in a line's last M-cycle;
  *  - the first line after the LCD is turned on has no mode 2: it reads as mode 0 until the
  *    transfer, and OAM is not locked (lcdon_timing);
  *  - entering VBlank also raises a mode 2 STAT interrupt (vblank_stat_intr);
  *  - VRAM and OAM are locked against the CPU as `vramReadLocked` and the others describe.
  * After the boot ROM the PPU is 98 M-cycles into line 153. */
class PpuReference(vram: Array[Int], oam: Array[Int], postBoot: Boolean) {
  var lcdc = if (postBoot) Gb.PostBoot.lcdc else 0
  var statSelect, scy, scx, lyc, wy, wx = 0
  var bgp = if (postBoot) Gb.PostBoot.bgp else 0
  var obp0, obp1 = if (postBoot) Gb.PostBoot.obp else 0
  /** M-cycle within the line (0-113) and the line. */
  var x = if (postBoot) Gb.PostBoot.ppuX else 0
  var ly = if (postBoot) Gb.PostBoot.ppuLy else 0
  var mode = 0
  /** LY = LYC, compared only while the LCD is on: turning it off freezes the flag. */
  var coincidence = false
  /** The LY = LYC interrupt line: while LY is not compared (the flag reads 0) it holds its value. */
  private var lycLine = false
  /** The first line after the LCD is turned on, which has no OAM scan: it reads as mode 0. */
  private var firstLine = false
  /** A STAT interrupt a halted CPU sees an M-cycle late, and one a running CPU does. */
  var statLate = false
  /** LY as read and compared: line 153 reads 0 after its first M-cycle. */
  def lyNow = if (ly == Gb.frameLines - 1 && x >= 1) 0 else ly
  /** Whether LY is compared with LYC this M-cycle: not while LY changes. */
  private def compared = (x != Gb.lineCycles - 1 || ly == Gb.frameLines - 1) && !(ly == Gb.frameLines - 1 && x == 1)
  /** The mode the STAT interrupt sees. */
  var irqMode = 0
  private var statLine = false

  // Objects found by the OAM scan.
  val objY, objX, objIndex = Array.fill(10)(0)
  var objCount = 0
  /** Objects fetched or passed in this line's transfer. */
  private var objDone = 0
  // Window
  private var wyHit = false
  // Pixel transfer
  private var transfer = false
  /** Dots the current or last pixel transfer has taken. */
  var transferDots = 0

  /** The last frame drawn, shades 0 (white) to 3, row-major 160 x 144. */
  val frame = Array.fill(160 * 144)(0)
  var frames = 0L
  /** Called with each completed frame, as VBlank starts. */
  var onFrame: Array[Int] => Unit = _ => ()
  /** Called with each pixel drawn (line, x, dot): dots count from 1, four per M-cycle tick, for
    * comparing dot timing against other emulators. */
  var onPixel: (Int, Int, Long) => Unit = null
  private var dots = 0L
  private var dotNow = 0L
  /** Dots since reset, up to the end of the last tick: when this M-cycle's write lands. */
  def time = dots

  /** The dot of a write's M-cycle before which a register write lands, or 5 for after the
    * M-cycle's dots. Outside the pixel transfer's lines everything lands after the dots. */
  def writeDot(a: Int, d: Int): Int = a match {
    case Gb.SCX => 1
    case Gb.WY | Gb.WX => 3
    case Gb.STAT | Gb.LYC => 5
    case Gb.LCDC if ((lcdc ^ d) & 0x80) != 0 => 5
    case _ => 2
  }
  private var scheduled, scheduledNext: Option[(Int, Int)] = None
  /** Writes that land in the next M-cycle's dots: the registers the pixel transfer reads. */
  def lateWrite(a: Int, d: Int) = a == Gb.SCY || a == Gb.SCX || a == Gb.BGP || a == Gb.OBP0 || a == Gb.OBP1 ||
    a == Gb.WX || a == Gb.LCDC && ((lcdc ^ d) & 0x84) == 0
  /** A CPU write to a PPU register in this M-cycle; it lands in `tick` (`writeDot`), this
    * M-cycle's or the next's (`lateWrite`). */
  def schedule(a: Int, d: Int): Unit = if (lateWrite(a, d)) scheduledNext = Some(a -> d) else scheduled = Some(a -> d)
  private def applyScheduled(): Unit = for ((a, d) <- scheduled) { scheduled = None; write(a, d); if (a == Gb.WX) wxJustChanged = true }

  def on = (lcdc & 0x80) != 0

  /** The CPU's accesses the PPU blocks after this M-cycle's tick: VRAM reads in mode 3 and the
    * last M-cycle of the OAM scan, VRAM writes in mode 3; OAM reads in modes 2 and 3 and from the
    * last M-cycle of the line before, OAM writes in modes 2 and 3 except the last M-cycle of the
    * OAM scan (Mooneye lcdon_timing and lcdon_write_timing). */
  def vramReadLocked = mode == 3 || mode == 2 && x == Gb.oamEnd - 1
  def vramWriteLocked = mode == 3
  def oamReadLocked = mode >= 2 || on && x == Gb.lineCycles - 1 && (ly < Gb.visibleLines - 1 || ly == Gb.frameLines - 1)
  def oamWriteLocked = mode == 3 || mode == 2 && x != Gb.oamEnd - 1
  /** The OAM row (8 bytes) the scan reads this M-cycle, for the OAM corruption bug, or -1. */
  def oamBugRow = if (mode == 2 && x < Gb.oamEnd - 1) x + 1 else -1

  def read(a: Int): Int = a match {
    case Gb.LCDC => lcdc
    case Gb.STAT => 0x80 | statSelect << 3 | (if (coincidence) 4 else 0) | mode
    case Gb.SCY => scy
    case Gb.SCX => scx
    case Gb.LY => if (on && x == Gb.lineCycles - 1) (if (ly == Gb.frameLines - 1) 0 else ly + 1) else lyNow
    case Gb.LYC => lyc
    case Gb.BGP => bgp
    case Gb.OBP0 => obp0
    case Gb.OBP1 => obp1
    case Gb.WY => wy
    case Gb.WX => wx
  }

  def write(a: Int, d: Int): Unit = a match {
    case Gb.LCDC => lcdc = d
    case Gb.STAT => statSelect = (d >> 3) & 0xF
    case Gb.SCY => scy = d
    case Gb.SCX => scx = d
    case Gb.LYC => lyc = d; if (on && compared) lycLine = lyNow == lyc
    case Gb.BGP => bgp = d
    case Gb.OBP0 => obp0 = d
    case Gb.OBP1 => obp1 = d
    case Gb.WY => wy = d
    case Gb.WX => wx = d
    case _ =>
  }

  private def lineStart(): Unit = {
    firstLine = false
    if (ly == 0) { wyHit = false; winY = -1 }
    objCount = 0
    transfer = false
    pixActive = false
  }

  /** One M-cycle. Returns the VBlank and STAT interrupt requests. */
  def tick(): (Boolean, Boolean) = {
    val r = tickDots()
    applyScheduled()
    scheduled = scheduledNext
    scheduledNext = None
    r
  }

  private def tickDots(): (Boolean, Boolean) = {
    // A STAT write on the DMG first enables every STAT interrupt source for a dot, as things
    // stood after the last M-cycle, except mode 2's at the end of HBlank when only mode 0's was
    // enabled (SameBoy).
    val glitchSelect = if (!scheduled.exists(_._1 == Gb.STAT)) 0
      else if (irqMode == 2 && x == Gb.lineCycles - 1 && (statSelect & 5) == 1) 0xB else 0xF
    // The mode 2 source is only on as the line starts (SameBoy's mode_for_interrupt).
    // Neither is the first line's mode 0 before its transfer, after the LCD is turned on, nor
    // VBlank's in line 153's last M-cycle.
    val glitchMode = if (irqMode == 2 && x != Gb.lineCycles - 1 && !(x == 0 && ly == 0) ||
      firstLine && x <= Gb.oamEnd ||
      ly == Gb.frameLines - 1 && x == Gb.lineCycles - 1) -1 else irqMode
    dots += 4
    val vblank = on && ly == Gb.visibleLines - 1 && x == Gb.lineCycles - 1
    if (vblank) { frames += 1; onFrame(frame) }
    if (!on) { x = 0; ly = 0; lineStart(); firstLine = true }
    else if (x == Gb.lineCycles - 1) { x = 0; ly = if (ly == Gb.frameLines - 1) 0 else ly + 1; lineStart() }
    else x += 1

    mode = 0
    irqMode = 0
    // The STAT interrupt's mode before this M-cycle's dots, for a halted CPU.
    var irqModeBefore = -1
    if (on && ly >= Gb.visibleLines) {
      // The last M-cycle of the frame reads as mode 0.
      mode = if (ly == Gb.frameLines - 1 && x == Gb.lineCycles - 1) 0 else 1
      irqMode = 1
    }
    else if (on) {
      if (ly == wy && (lcdc & 0x20) != 0) wyHit = true
      val scanMode = if (firstLine) 0 else 2
      // The first line's mode 0 before its transfer is no STAT interrupt source.
      val scanIrqMode = if (firstLine) 3 else 2
      if (x < Gb.oamEnd) {
        scan(2 * x); scan(2 * x + 1)
        mode = scanMode; irqMode = scanIrqMode
      } else {
        if (x == Gb.oamEnd) startTransfer()
        val transferBefore = transfer
        irqModeBefore = if (x == Gb.oamEnd) scanIrqMode else if (transfer) 3 else 0
        val active = Array.fill(8)(false)
        for (i <- 1 to 4) {
          dotNow = dots - 4 + i
          for ((a, d) <- scheduled if i >= writeDot(a, d)) {
            if (a == Gb.LCDC && (lcdc & 2) != 0 && (d & 2) == 0 && duringObjFetch) abortObj = true
            applyScheduled()
          }
          for ((a, d) <- scheduled if i == writeDot(a, d) - 1) glitch(a, d)
          if (pixActive) sbDot()
          wxJustChanged = false
          active(i) = pixActive
        }
        // Mode 3 ends 3 dots before the transfer does: look 3 dots past this M-cycle.
        if (pixActive) {
          val saved = snapshot()
          dry = true
          for (i <- 5 to 7) { if (pixActive) sbDot(); active(i) = pixActive }
          dry = false
          restore(saved)
        }
        def transferAfter(i: Int) = transferBefore && active(i + 3)
        irqMode = if (x == Gb.oamEnd) scanIrqMode
          // The first line's mode 0 interrupt sees the transfer as it was before the dots.
          else if (if (firstLine) transferBefore else transferAfter(2)) 3 else 0
        mode = if (transferAfter(3)) 3 else 0
        transfer = transferAfter(4)
      }
    }
    // The LY = LYC flag drops in the line's last M-cycle, where LY already reads the next line
    // (except at the end of the frame, where LY stays 0), and in line 153's second M-cycle,
    // where LY changes to 0.
    val lycHeld = lycLine
    if (on) {
      coincidence = compared && lyNow == lyc
      if (compared) lycLine = coincidence
    }
    // Entering VBlank also counts as the start of a mode 2 for the STAT interrupt.
    // The next line's mode 2 interrupt rises with LY, except line 0's.
    if (on && x == Gb.lineCycles - 1 && ly < Gb.visibleLines - 1) irqMode = 2
    val vblankMode2 = ly == Gb.visibleLines && x == 0
    def statFor(m: Int, lyc: Boolean = lycLine) = ((statSelect & 8) != 0 && lyc) || on && (((statSelect & 4) != 0 && (m == 2 || vblankMode2)) ||
      ((statSelect & 2) != 0 && m == 1) || ((statSelect & 1) != 0 && m == 0))
    val line = statFor(irqMode)
    // The held LY = LYC line still counts at this M-cycle's first dot, against a STAT just written.
    val writeRise = on && (((glitchSelect & 8) != 0 && lycHeld) || ((glitchSelect & 4) != 0 && glitchMode == 2) ||
      ((glitchSelect & 2) != 0 && glitchMode == 1) || ((glitchSelect & 1) != 0 && glitchMode == 0))
    val start = statLine || (statSelect & 8) != 0 && lycHeld || writeRise
    // Entering VBlank, LY = LYC is compared before the mode changes.
    val mid = if (vblankMode2) statFor(0) else line
    val stat = !statLine && start || !start && mid || !mid && line
    // A halted CPU sees the STAT line as it was before this M-cycle's dots.
    statLate = stat && !(writeRise && !statLine) && !statFor(if (irqModeBefore < 0) irqMode else irqModeBefore, lycLine || lycHeld)
    statLine = line
    (vblank, stat)
  }

  def objHeight = if ((lcdc & 4) != 0) 16 else 8

  private def scan(e: Int): Unit = {
    val y = oam(4 * e)
    if (objCount < 10 && ly + 16 >= y && ly + 16 < y + objHeight) {
      objY(objCount) = y; objX(objCount) = oam(4 * e + 1); objIndex(objCount) = e
      objCount += 1
    }
  }

  private def startTransfer(): Unit = {
    transfer = true
    transferDots = 0
    bgSize = 8; java.util.Arrays.fill(bgF, 0)
    obSize = 0
    lcdX = 0; pos = -16; pixActive = true
    fSt = 0; sbSt = 0; sbWait = 7
    insertBg = false; noInsertGlitch = false; wxTriggered = false; abortObj = false; duringObjFetch = false
    objDone = 0
  }

  // ---- The pixel transfer: SameBoy's DMG FIFO and fetcher --------------------------------------
  private var winY = -1
  private var pixActive = false
  /** The `sbStep` state, and dots to wait before the next step. */
  private var sbSt, sbWait = 0
  private var pos = -16
  private var lcdX = 0
  // FIFOs: index 0 leaves first.
  private val bgF = new Array[Int](8); private var bgSize = 0
  private val obPix, obPal, obPrio = new Array[Int](8); private var obSize = 0
  private var fSt, tileIdxAddr, curTile, tileDataAddr, data0, data1 = 0
  private var wxTriggered, winFetching, insertBg, noInsertGlitch, wxJustChanged, duringObjFetch, abortObj = false
  private var winTileX = 0
  private var oTile, oFlags, sbObjLo, fetchSlot = 0
  private var lastShade = 0
  /** Looking ahead (`tickDots`): no pixels go out. */
  private var dry = false

  /** The value a palette or LCDC write shows for the dot before it lands. */
  private def glitch(a: Int, d: Int): Unit = a match {
    case Gb.BGP | Gb.OBP0 | Gb.OBP1 => write(a, read(a) | d)
    case Gb.LCDC if ((lcdc ^ d) & 0x80) == 0 =>
      var old = lcdc
      if ((pos == 0 || duringObjFetch) && (d & 2) == 0) {
        if ((old & 2) != 0 && duringObjFetch) abortObj = true
        old &= ~2
      }
      if ((old & 0x20) != 0 && (d & 0x20) == 0 && winFetching) noInsertGlitch = true
      lcdc = old | (d & 1)
    case _ =>
  }

  private def bgPop(): Int = { val v = bgF(0); shift(bgF); bgSize -= 1; v }
  private def shift(a: Array[Int]): Unit = { for (k <- 0 until 7) a(k) = a(k + 1); a(7) = 0 }
  private def xMatch = if (pos + 8 < 0) 0 else pos + 8
  /** The next object to fetch at the current X, in OAM order, or -1. */
  private def pendingObj = if ((lcdc & 2) == 0) -1 else (0 until objCount).find(i => (objDone & (1 << i)) == 0 && objX(i) == xMatch).getOrElse(-1)
  private def fetcherY = if (wxTriggered) winY & 0xFF else (ly + scy) & 0xFF

  private def advance(): Unit = fSt match {
    case 0 =>
      if ((lcdc & 0x20) == 0) wxTriggered = false
      val map = if (!wxTriggered && (lcdc & 8) != 0 || wxTriggered && (lcdc & 0x40) != 0) 0x1C00 else 0x1800
      val y = fetcherY
      val x = if (wxTriggered) winTileX else if (pos < -8) scx >> 3 else ((scx + pos + 8) >> 3) & 0x1F
      tileIdxAddr = map + x + (y >> 3) * 32
      fSt = 1
    case 1 => curTile = vram(tileIdxAddr); fSt = 2
    case 2 | 4 =>
      val base = if ((lcdc & 0x10) != 0) curTile * 16 else curTile.toByte * 16 + 0x1000
      tileDataAddr = base + (fetcherY & 7) * 2 + (if (fSt == 4) 1 else 0)
      fSt += 1
    case 3 => data0 = vram(tileDataAddr); fSt = 4
    case 5 =>
      data1 = vram(tileDataAddr)
      if (wxTriggered) winTileX = (winTileX + 1) & 0x1F
      push()
    case _ => push()
  }

  private def push(): Unit = {
    fSt = 6
    if (bgSize > 0) return
    if (wyHit && (lcdc & 0x20) == 0 && !noInsertGlitch) {
      val logical = if (pos + 7 > 167) 0 else pos + 7
      if (wx == logical) { bgF(0) = 0; bgSize = 1; return }
    }
    for (b <- 0 until 8) bgF(b) = (if ((data1 << b & 0x80) != 0) 2 else 0) | (if ((data0 << b & 0x80) != 0) 1 else 0)
    bgSize = 8
    fSt = 0
  }

  private def render(): Unit = {
    if ((lcdc & 2) != 0 && (0 until objCount).exists(i => (objDone & (1 << i)) == 0 && objX(i) == 0)) return
    if (bgSize == 0) return
    val bg = if (insertBg) { insertBg = false; 0 } else bgPop()
    var draw = false; var op, opal, oprio = 0
    if (obSize > 0) {
      op = obPix(0); opal = obPal(0); oprio = obPrio(0)
      shift(obPix); shift(obPal); shift(obPrio); obSize -= 1
      draw = op != 0 && (lcdc & 2) != 0
    }
    if (pos >= -16 && pos < -8) {
      if ((pos & 7) == (scx & 7)) pos = -8
      else if (winFetching && (pos & 7) == 6 && (scx & 7) == 7) pos = -8
      else if (pos == -9) { pos = -16; return }
    }
    winFetching = false
    if (pos < 0 || pos >= 160) { pos += 1; return }
    val c = if ((lcdc & 1) != 0) bg else 0
    if (draw && c != 0 && oprio != 0) draw = false
    val shade = if (draw) ((if (opal != 0) obp1 else obp0) >> (2 * op)) & 3 else (bgp >> (2 * c)) & 3
    if (lcdX < 160 && !dry) {
      frame(ly * 160 + lcdX) = shade
      if (onPixel != null) onPixel(ly, lcdX, dotNow)
    }
    lastShade = shade
    pos += 1; lcdX += 1
  }

  private def sbDot(): Unit = {
    transferDots += 1
    if (abortObj) { abortObj = false; if (sbSt >= 1 && sbSt <= 6) { sbSt = 7; sbWait = 0 } }
    if (sbWait > 0) sbWait -= 1
    else {
      sbWait = -1
      while (pixActive && sbWait < 0) sbStep()
    }
  }

  /** One run of SameBoy's mode 3 loop up to its next sleep, which sets `sbWait`. */
  private def sbStep(): Unit = sbSt match {
    case 0 =>
      if (!wxTriggered && wyHit && (lcdc & 0x20) != 0) {
        val act = if (wx == 0) pos == -7 || pos == -16 && (scx & 7) != 0 || pos >= -15 && pos <= -8
          else if (wx < 166) wx == ((pos + 7) & 0xFF) || wx == ((pos + 6) & 0xFF) && !wxJustChanged && { if (lcdX > 0) lcdX -= 1; true }
          else false
        if (act) {
          winY += 1; winTileX = 0; bgSize = 0
          wxTriggered = true; fSt = 0; winFetching = true
          if (wx == 0 && (scx & 7) != 0) { sbSt = 8; sbWait = 0; return }
        } else if (wx == 166 && wx == ((pos + 7) & 0xFF)) winY += 1
      }
      sbSt = 9
    case 8 => sbSt = 9 // after the WX = 0 delay
    case 9 =>
      if (wx == ((pos + 7) & 0xFF) && wxTriggered && !winFetching && fSt == 0 && bgSize == 8) insertBg = true
      for (i <- 0 until objCount if objX(i) < xMatch) objDone |= 1 << i
      duringObjFetch = true
      sbSt = if (pendingObj >= 0) 1 else 7
    case 1 =>
      // Finish the background tile before the object; state 2 is entered in the same dot.
      sbSt = if (fSt < 5 || bgSize == 0) 1 else 3
      advance(); sbWait = 0
    case 3 =>
      advance()
      fetchSlot = pendingObj
      val e = objIndex(fetchSlot)
      oTile = oam(4 * e + 2); oFlags = oam(4 * e + 3)
      sbSt = 4; sbWait = 1
    case 4 =>
      sbObjLo = vram(objLine(fetchSlot)); sbSt = 5; sbWait = 1
    case 5 =>
      duringObjFetch = false
      overlay(sbObjLo, vram(objLine(fetchSlot) + 1))
      objDone |= 1 << fetchSlot
      sbSt = 6; sbWait = 0
    case 6 => sbSt = if (pendingObj >= 0) 1 else 7
    case 7 =>
      duringObjFetch = false
      render()
      advance()
      if (pos == 160) {
        pixActive = false
        while (lcdX < 160) { if (!dry) frame(ly * 160 + lcdX) = lastShade; lcdX += 1 }
        wxTriggered = false
      } else { sbSt = 0; sbWait = 0 }
  }

  private def objLine(i: Int): Int = {
    val h16 = (lcdc & 4) != 0
    var y = (ly - objY(i)) & (if (h16) 0xF else 7)
    if ((oFlags & 0x40) != 0) y ^= (if (h16) 0xF else 7)
    (if (h16) oTile & 0xFE else oTile) * 16 + y * 2
  }

  private def overlay(lo: Int, hi: Int): Unit = {
    while (obSize < 8) { obPix(obSize) = 0; obPal(obSize) = 0; obPrio(obSize) = 0; obSize += 1 }
    val flip = (oFlags & 0x20) != 0
    for (b <- 0 until 8) {
      val p = (if ((hi << b & 0x80) != 0) 2 else 0) | (if ((lo << b & 0x80) != 0) 1 else 0)
      val k = if (flip) 7 - b else b
      if (p != 0 && obPix(k) == 0) { obPix(k) = p; obPal(k) = (oFlags >> 4) & 1; obPrio(k) = (oFlags >> 7) & 1 }
    }
  }

  // The transfer state the look-ahead changes.
  private def snapshot() = (transferDots, winY, pixActive, sbSt, sbWait, pos, lcdX, bgF.clone, bgSize,
    (obPix.clone, obPal.clone, obPrio.clone, obSize), (fSt, tileIdxAddr, curTile, tileDataAddr, data0, data1),
    (wxTriggered, winFetching, insertBg, noInsertGlitch, wxJustChanged, duringObjFetch, abortObj),
    (winTileX, objDone, oTile, oFlags, sbObjLo, lastShade, fetchSlot))
  private def restore(s: ReturnType): Unit = {
    transferDots = s._1; winY = s._2; pixActive = s._3; sbSt = s._4; sbWait = s._5; pos = s._6; lcdX = s._7
    Array.copy(s._8, 0, bgF, 0, 8); bgSize = s._9
    Array.copy(s._10._1, 0, obPix, 0, 8); Array.copy(s._10._2, 0, obPal, 0, 8); Array.copy(s._10._3, 0, obPrio, 0, 8)
    obSize = s._10._4
    fSt = s._11._1; tileIdxAddr = s._11._2; curTile = s._11._3; tileDataAddr = s._11._4; data0 = s._11._5; data1 = s._11._6
    wxTriggered = s._12._1; winFetching = s._12._2; insertBg = s._12._3; noInsertGlitch = s._12._4
    wxJustChanged = s._12._5; duringObjFetch = s._12._6; abortObj = s._12._7
    winTileX = s._13._1; objDone = s._13._2; oTile = s._13._3; oFlags = s._13._4; sbObjLo = s._13._5; lastShade = s._13._6; fetchSlot = s._13._7
  }
  private type ReturnType = (Int, Int, Boolean, Int, Int, Int, Int, Array[Int], Int, (Array[Int], Array[Int], Array[Int], Int),
    (Int, Int, Int, Int, Int, Int), (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean), (Int, Int, Int, Int, Int, Int, Int))
}

