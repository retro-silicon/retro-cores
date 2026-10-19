package retro.gb

/** Dot-level model of the DMG PPU for `GbReference`, written from the Pan Docs pixel FIFO
  * description; `PpuPlugin` is its RTL. Each M-cycle is four dots; `tick` runs them.
  *
  * A line is 456 dots: OAM scan (80 dots, two OAM entries per M-cycle, up to ten objects), pixel
  * transfer, then HBlank. Lines 144-153 are VBlank. In the transfer the background fetcher (tile
  * number, low byte, high byte, two dots each) fills an 8-pixel FIFO whenever it is empty, and
  * one pixel leaves the FIFO per dot. The first tile is fetched in 4 dots and its 8 pixels are
  * dropped (they lie left of the screen, where objects with X < 8 start), as are SCX mod 8 more,
  * so the transfer lasts 172 dots plus:
  *  - SCX mod 8, the dropped pixels;
  *  - 6 for the window: reaching WX - 7 empties the FIFO and restarts the fetcher on the window;
  *  - for each object reached, 6 dots of object fetch, after the background fetcher has finished
  *    the tile it is on (up to 5 more dots). The object's pixels mix into an object FIFO where it
  *    is transparent, so an object fetched earlier (lower X, or earlier in OAM) wins.
  * This reproduces Pan Docs' object penalty formula and all 105 cases of Mooneye's
  * intr_2_mode0_timing_sprites.
  *
  * Timing seen by the CPU, as Mooneye's PPU tests pin it down:
  *  - STAT reads the mode after the M-cycle's third dot, so a transfer of L dots reads as mode 3
  *    for ceil((L - 3) / 4) M-cycles from M-cycle 20 of the line;
  *  - the STAT interrupt sees the transfer as it was before the M-cycle's dots, so its mode 0
  *    request comes ceil(L / 4) M-cycles after M-cycle 20 (hblank_ly_scx_timing);
  *  - LY reads the next line in the last M-cycle of a line;
  *  - the LY = LYC flag is only updated while the LCD is on, and stays on the STAT interrupt line
  *    while it is off (stat_lyc_onoff); it drops in a line's last M-cycle;
  *  - the first line after the LCD is turned on has no mode 2: it reads as mode 0 until the
  *    transfer, and OAM is not locked (lcdon_timing);
  *  - entering VBlank also raises a mode 2 STAT interrupt (vblank_stat_intr);
  *  - VRAM and OAM are locked against the CPU as `vramReadLocked` and the others describe.
  *
  * Not modelled: LY reading 0 early in line 153, and register writes landing mid-dot. */
class PpuReference(vram: Array[Int], oam: Array[Int], postBoot: Boolean) {
  var lcdc = if (postBoot) Gb.PostBoot.lcdc else 0
  var statSelect, scy, scx, lyc, bgp, obp0, obp1, wy, wx = 0
  /** M-cycle within the line (0-113) and the line. */
  var x, ly = 0
  var mode = 0
  /** LY = LYC, compared only while the LCD is on: turning it off freezes the flag. */
  var coincidence = false
  /** The first line after the LCD is turned on, which has no OAM scan: it reads as mode 0. */
  private var firstLine = false
  /** The mode the STAT interrupt sees. */
  var irqMode = 0
  private var statLine = false

  // Objects found by the OAM scan.
  val objY, objX, objTile, objAttr = Array.fill(10)(0)
  var objCount = 0
  private var fetched = 0
  // Window
  private var wyHit, winUsed = false
  var winLine = 0
  // Pixel transfer
  private var transfer = false
  /** Dots the current or last pixel transfer has taken. */
  var transferDots = 0
  private var pos8, discard = 0
  private var fStep, fTileX, fTile, fLo, fHi = 0
  private var fWindow, winActive = false
  private var bgLo, bgHi, bgCount = 0
  private var oLo, oHi, oPal, oPrio = 0
  private var sActive = false
  private var sStep, sSlot, sLo = 0

  /** The last frame drawn, shades 0 (white) to 3, row-major 160 x 144. */
  val frame = Array.fill(160 * 144)(0)
  var frames = 0L
  /** Called with each completed frame, as VBlank starts. */
  var onFrame: Array[Int] => Unit = _ => ()

  def on = (lcdc & 0x80) != 0

  /** The CPU's accesses the PPU blocks after this M-cycle's tick: VRAM reads in mode 3 and the
    * last M-cycle of the OAM scan, VRAM writes in mode 3; OAM reads in modes 2 and 3 and from the
    * last M-cycle of the line before, OAM writes in modes 2 and 3 except the last M-cycle of the
    * OAM scan (Mooneye lcdon_timing and lcdon_write_timing). */
  def vramReadLocked = mode == 3 || mode == 2 && x == Gb.oamEnd - 1
  def vramWriteLocked = mode == 3
  def oamReadLocked = mode >= 2 || on && x == Gb.lineCycles - 1 && (ly < Gb.visibleLines - 1 || ly == Gb.frameLines - 1)
  def oamWriteLocked = mode == 3 || mode == 2 && x != Gb.oamEnd - 1

  def read(a: Int): Int = a match {
    case Gb.LCDC => lcdc
    case Gb.STAT => 0x80 | statSelect << 3 | (if (coincidence) 4 else 0) | mode
    case Gb.SCY => scy
    case Gb.SCX => scx
    case Gb.LY => if (on && x == Gb.lineCycles - 1) (if (ly == Gb.frameLines - 1) 0 else ly + 1) else ly
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
    case Gb.LYC => lyc = d
    case Gb.BGP => bgp = d
    case Gb.OBP0 => obp0 = d
    case Gb.OBP1 => obp1 = d
    case Gb.WY => wy = d
    case Gb.WX => wx = d
    case _ =>
  }

  private def lineStart(): Unit = {
    firstLine = false
    if (ly == 0) { wyHit = false; winLine = 0 }
    else if (winUsed) winLine = (winLine + 1) & 0xFF
    winUsed = false
    objCount = 0
    fetched = 0
    transfer = false
  }

  /** One M-cycle. Returns the VBlank and STAT interrupt requests. */
  def tick(): (Boolean, Boolean) = {
    val vblank = on && ly == Gb.visibleLines - 1 && x == Gb.lineCycles - 1
    if (vblank) { frames += 1; onFrame(frame) }
    if (!on) { x = 0; ly = 0; lineStart(); firstLine = true }
    else if (x == Gb.lineCycles - 1) { x = 0; ly = if (ly == Gb.frameLines - 1) 0 else ly + 1; lineStart() }
    else x += 1

    mode = 0
    irqMode = 0
    if (on && ly >= Gb.visibleLines) { mode = 1; irqMode = 1 }
    else if (on) {
      if (ly == wy) wyHit = true
      val scanMode = if (firstLine) 0 else 2
      if (x < Gb.oamEnd) {
        scan(2 * x); scan(2 * x + 1)
        mode = scanMode; irqMode = scanMode
      } else {
        irqMode = if (x == Gb.oamEnd) scanMode else if (transfer) 3 else 0
        if (x == Gb.oamEnd) startTransfer()
        for (i <- 1 to 4) {
          if (transfer) dot()
          if (i == 3) mode = if (transfer) 3 else 0
        }
      }
    }
    // The LY = LYC flag drops in the line's last M-cycle, where LY already reads the next line.
    if (on) coincidence = ly == lyc && x != Gb.lineCycles - 1
    // Entering VBlank also counts as the start of a mode 2 for the STAT interrupt.
    val vblankMode2 = ly == Gb.visibleLines && x == 0
    val line = ((statSelect & 8) != 0 && coincidence) || on && (((statSelect & 4) != 0 && (irqMode == 2 || vblankMode2)) ||
      ((statSelect & 2) != 0 && irqMode == 1) || ((statSelect & 1) != 0 && irqMode == 0))
    val stat = line && !statLine
    statLine = line
    (vblank, stat)
  }

  def objHeight = if ((lcdc & 4) != 0) 16 else 8

  private def scan(e: Int): Unit = {
    val y = oam(4 * e)
    if (objCount < 10 && ly + 16 >= y && ly + 16 < y + objHeight) {
      objY(objCount) = y; objX(objCount) = oam(4 * e + 1)
      objTile(objCount) = oam(4 * e + 2); objAttr(objCount) = oam(4 * e + 3)
      objCount += 1
    }
  }

  private def startTransfer(): Unit = {
    transfer = true
    transferDots = 0
    pos8 = 0; discard = scx & 7
    fStep = 2; fTileX = 0x1F; fTile = 0; fWindow = false; winActive = false
    bgCount = 0; oLo = 0; oHi = 0; oPal = 0; oPrio = 0
    sActive = false
  }

  /** VRAM index of a background or window tile's row byte. */
  private def tileData(tile: Int, row: Int, high: Int) =
    (if ((lcdc & 0x10) == 0 && (tile & 0x80) == 0) 0x1000 else 0) + tile * 16 + row * 2 + high

  private def bgRow = if (fWindow) winLine else (ly + scy) & 0xFF

  private def objRow(s: Int, high: Int) = {
    val r = (ly + 16 - objY(s)) & 0xF
    val row = if ((objAttr(s) & 0x40) != 0) objHeight - 1 - r else r
    val tile = if (objHeight == 16) objTile(s) & 0xFE else objTile(s)
    val b = vram(tile * 16 + (row & 0xF) * 2 + high)
    if ((objAttr(s) & 0x20) != 0) Integer.reverse(b) >>> 24 else b
  }

  private def dot(): Unit = {
    transferDots += 1
    if (sActive) {
      sStep += 1
      if (sStep == 4) sLo = objRow(sSlot, 0)
      if (sStep == 6) {
        val hi = objRow(sSlot, 1)
        val pal = (objAttr(sSlot) >> 4) & 1
        val prio = (objAttr(sSlot) >> 7) & 1
        for (b <- 0 until 8) {
          val m = 1 << b
          if (((oLo | oHi) & m) == 0) {
            oLo = oLo & ~m | sLo & m
            oHi = oHi & ~m | hi & m
            oPal = oPal & ~m | (if (pal != 0) m else 0)
            oPrio = oPrio & ~m | (if (prio != 0) m else 0)
          }
        }
        fetched |= 1 << sSlot
        sActive = false
      }
      return
    }
    // Reaching WX - 7 restarts the fetcher on the window; this dot is its first step.
    if (!winActive && (lcdc & 0x20) != 0 && wyHit && discard == 0 && pos8 == wx + 1) {
      winActive = true; winUsed = true; fWindow = true
      fTileX = 0; fStep = 1; bgCount = 0
      return
    }
    // Background fetcher. A push also counts as the next fetch's first step.
    fStep match {
      case 6 =>
        if (bgCount == 0) {
          bgLo = fLo; bgHi = fHi; bgCount = 8
          fTileX = (fTileX + 1) & 0x1F
          fStep = 1
        }
      case s =>
        if (s == 1) {
          val base = if ((lcdc & (if (fWindow) 0x40 else 0x08)) != 0) 0x1C00 else 0x1800
          val col = if (fWindow) fTileX else ((scx >> 3) + fTileX) & 0x1F
          fTile = vram(base + (bgRow >> 3) * 32 + col)
        }
        if (s == 3) fLo = vram(tileData(fTile, bgRow & 7, 0))
        if (s == 5) fHi = vram(tileData(fTile, bgRow & 7, 1))
        fStep = s + 1
    }
    // An object starting here stops the pixels until the fetcher has its tile, then is fetched.
    if ((lcdc & 2) != 0 && discard == 0) {
      val hit = (0 until objCount).find(i => (fetched & (1 << i)) == 0 && objX(i) == pos8)
      for (i <- hit) {
        if (bgCount > 0 && fStep == 6) { sActive = true; sStep = 1; sSlot = i }
        return
      }
    }
    if (bgCount == 0) return
    val bg = (if ((bgHi & 0x80) != 0) 2 else 0) | (if ((bgLo & 0x80) != 0) 1 else 0)
    bgLo = (bgLo << 1) & 0xFF; bgHi = (bgHi << 1) & 0xFF; bgCount -= 1
    if (discard > 0) { discard -= 1; return }
    val obj = (if ((oHi & 0x80) != 0) 2 else 0) | (if ((oLo & 0x80) != 0) 1 else 0)
    val pal = (oPal >> 7) & 1; val prio = (oPrio >> 7) & 1
    oLo = (oLo << 1) & 0xFF; oHi = (oHi << 1) & 0xFF; oPal = (oPal << 1) & 0xFF; oPrio = (oPrio << 1) & 0xFF
    val c = if ((lcdc & 1) != 0) bg else 0
    if (pos8 >= 8) frame(ly * 160 + pos8 - 8) =
      if ((lcdc & 2) != 0 && obj != 0 && !(prio != 0 && c != 0)) ((if (pal != 0) obp1 else obp0) >> (2 * obj)) & 3
      else (bgp >> (2 * c)) & 3
    pos8 += 1
    if (pos8 == 168) transfer = false
  }
}
