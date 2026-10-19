package retro.nes

/** Dot-level model of the NTSC 2C02 PPU: 341 dots by 262 lines, the background fetch pipeline
  * with its shift registers, sprite evaluation into secondary OAM (with the overflow flag's
  * scanning bug), sprite 0 hit, the VBlank flag and NMI output, the odd-frame skipped dot, and
  * the CPU-visible registers 2000-2007 with the loopy v/t/x/w scroll registers and the 2007 read
  * buffer. Written first so test ROMs run in seconds; the RTL `PpuPlugin` mirrors it.
  *
  * Each `tick()` is one dot. `frame` holds the last 256x240 frame as palette indices (0-63). */
class NesPpuReference(cart: NesCartridge) {
  var ctrl, mask = 0
  var vblank, sprite0Hit, overflow = false
  var oamAddr = 0
  val oam = Array.fill(256)(0)
  var v, t, fineX = 0
  var w = false
  var buffer = 0
  /** The PPU's data bus latch: what unused register bits read as. */
  var latch = 0
  /** Palette RAM, as blargg's console has it at power-up (power_up_palette checks this). */
  val palette = Array(0x09, 0x01, 0x00, 0x01, 0x00, 0x02, 0x02, 0x0D, 0x08, 0x10, 0x08, 0x24, 0x00, 0x00, 0x04, 0x2C,
                      0x09, 0x01, 0x34, 0x03, 0x00, 0x04, 0x00, 0x14, 0x08, 0x3A, 0x00, 0x02, 0x00, 0x20, 0x2C, 0x08)
  val ciram = Array.fill(0x800)(0)

  var dot, line = 0
  var oddFrame = false
  var frames = 0L
  val frame = Array.fill(256 * 240)(0)
  /** Called with `frame` each time one completes (at VBlank). */
  var onFrame: Array[Int] => Unit = _ => ()
  private val back = Array.fill(256 * 240)(0)
  /** Set by a 2002 read in the dot before VBlank starts: the flag is not set this frame. */
  private var suppress = false
  private var skipDot = false

  def rendering = (mask & 0x18) != 0
  def nmi = vblank && (ctrl & 0x80) != 0

  // ---- PPU address space ---------------------------------------------------------------------
  /** Dots since power-up, for the cartridge's view of the PPU bus. */
  var dotCount = 0L
  def read(address: Int): Int = {
    val a = address & 0x3FFF
    cart.ppuBus(a, dotCount)
    if (a < 0x2000) cart.chr(cart.chrIndex(a)) else if (a < 0x3F00) ciram(cart.ciram(a)) else palette(paletteIndex(a))
  }
  def write(address: Int, value: Int): Unit = {
    val a = address & 0x3FFF
    cart.ppuBus(a, dotCount)
    if (a < 0x2000) { if (cart.chrRam) cart.chr(cart.chrIndex(a)) = value }
    else if (a < 0x3F00) ciram(cart.ciram(a)) = value
    else palette(paletteIndex(a)) = value & 0x3F
  }
  private def paletteIndex(a: Int) = { val i = a & 0x1F; if ((i & 0x13) == 0x10) i & 0x0F else i }

  // ---- CPU registers -------------------------------------------------------------------------
  /** The frame each latch bit was last refreshed: a bit not refreshed for about 600 ms (36
    * frames) decays to 0. */
  private val latchFrame = Array.fill(8)(0L)
  private def decayed = (0 until 8).map(b => if (frames - latchFrame(b) > 36) 0 else latch & (1 << b)).sum
  /** Drives `value` onto the bits in `mask`; the other bits read as the decaying latch. */
  private def drive(value: Int, mask: Int): Int = {
    val r = (value & mask) | (decayed & ~mask & 0xFF)
    for (b <- 0 until 8 if (mask >> b & 1) != 0) latchFrame(b) = frames
    latch = r
    r
  }

  def cpuRead(register: Int): Int = register & 7 match {
    case 2 =>
      val s = (if (vblank) 0x80 else 0) | (if (sprite0Hit) 0x40 else 0) | (if (overflow) 0x20 else 0)
      vblank = false
      w = false
      if (line == 241 && dot == 1) suppress = true
      drive(s, 0xE0)
    case 4 => drive(oam(oamAddr), 0xFF)
    case 7 =>
      val a = v & 0x3FFF
      val r = if (a >= 0x3F00) { buffer = ciram(cart.ciram(a)); drive(palette(paletteIndex(a)) & (if ((mask & 1) != 0) 0x30 else 0x3F), 0x3F) }
              else { val b = buffer; buffer = read(a); drive(b, 0xFF) }
      incrementV()
      if (!renderingNow) cart.ppuBus(v, dotCount)
      r
    case _ => decayed
  }

  def cpuWrite(register: Int, value: Int): Unit = {
    drive(value, 0xFF)
    register & 7 match {
      case 0 => ctrl = value; t = (t & ~0x0C00) | ((value & 3) << 10)
      case 1 => mask = value
      case 2 =>
      case 3 => oamAddr = value
      case 4 => oam(oamAddr) = if ((oamAddr & 3) == 2) value & 0xE3 else value; oamAddr = (oamAddr + 1) & 0xFF
      case 5 =>
        if (!w) { t = (t & ~0x1F) | (value >> 3); fineX = value & 7 }
        else t = (t & ~0x73E0) | ((value & 7) << 12) | ((value & 0xF8) << 2)
        w = !w
      case 6 =>
        if (!w) t = (t & 0x00FF) | ((value & 0x3F) << 8)
        else { t = (t & 0x7F00) | value; v = t; if (!renderingNow) cart.ppuBus(v, dotCount) }
        w = !w
      case 7 => write(v, value); incrementV(); if (!renderingNow) cart.ppuBus(v, dotCount)
    }
  }
  private def incrementV() = v = (v + (if ((ctrl & 4) != 0) 32 else 1)) & 0x7FFF

  // ---- Rendering state -------------------------------------------------------------------------
  private var ntByte, atByte, patLo, patHi = 0
  private var bgLo, bgHi, atLo, atHi = 0 // 16-bit pattern and attribute shifters
  private val spriteY, spriteTile, spriteAttr, spriteX = Array.fill(8)(0)
  private val spriteLo, spriteHi = Array.fill(8)(0)
  private var spriteCount = 0
  /** Secondary OAM: what evaluation found for the next line, until dot 257 loads it. */
  private val nextY, nextTile, nextAttr, nextX = Array.fill(8)(0)
  private var nextCount = 0
  private var sprite0Next, sprite0Line = false
  private def spriteHeight = if ((ctrl & 0x20) != 0) 16 else 8

  private def incX() = if ((v & 0x1F) == 31) v = (v & ~0x1F) ^ 0x400 else v += 1
  private def incY() = {
    if ((v & 0x7000) != 0x7000) v += 0x1000
    else {
      v &= ~0x7000
      var y = (v >> 5) & 0x1F
      if (y == 29) { y = 0; v ^= 0x800 } else if (y == 31) y = 0 else y += 1
      v = (v & ~0x3E0) | (y << 5)
    }
  }
  private def reload() = {
    bgLo = (bgLo & 0xFF00) | patLo; bgHi = (bgHi & 0xFF00) | patHi
    atLo = (atLo & 0xFF00) | (if ((atByte & 1) != 0) 0xFF else 0); atHi = (atHi & 0xFF00) | (if ((atByte & 2) != 0) 0xFF else 0)
  }
  private def shift() = { bgLo = (bgLo << 1) & 0xFFFF; bgHi = (bgHi << 1) & 0xFFFF; atLo = (atLo << 1) & 0xFFFF; atHi = (atHi << 1) & 0xFFFF }

  /** The dot at which this line's evaluation finds a ninth sprite and sets the overflow flag. */
  private var overflowDot = -1

  /** Sprite evaluation for the next line, done in one go at dot 65 when it starts, but with the
    * overflow flag set at the dot the hardware's scan reaches it: 2 dots per sprite checked, 8
    * per sprite copied, then the buggy diagonal scan once eight are found. */
  private def evaluate(): Unit = {
    val h = spriteHeight
    nextCount = 0
    sprite0Next = false
    overflowDot = -1
    var n = 0
    var at = 65
    while (n < 64 && nextCount < 8) {
      val y = oam(n * 4)
      if (line - y >= 0 && line - y < h) {
        if (n == 0) sprite0Next = true
        nextY(nextCount) = y; nextTile(nextCount) = oam(n * 4 + 1)
        nextAttr(nextCount) = oam(n * 4 + 2); nextX(nextCount) = oam(n * 4 + 3)
        nextCount += 1
        at += 8
      } else at += 2
      n += 1
    }
    var m = 0
    var found = false
    while (n < 64 && !found && at <= 256) {
      val y = oam(n * 4 + m)
      if (line - y >= 0 && line - y < h) { found = true; overflowDot = at + 1 }
      else { n += 1; m = (m + 1) & 3; at += 2 }
    }
  }
  /** Dot 257: the sprites found become the ones the next line draws. */
  private def loadSprites(): Unit = {
    spriteCount = nextCount
    for (i <- 0 until 8) { spriteY(i) = nextY(i); spriteTile(i) = nextTile(i); spriteAttr(i) = nextAttr(i); spriteX(i) = nextX(i) }
  }
  /** Dots 257-320, 8 per sprite: the pattern's low plane is read on the sprite's 5th dot and
    * the high plane on its 7th. Unused slots get a transparent pattern. */
  private def fetchSprite(i: Int, high: Boolean): Unit = {
    val row0 = line - spriteY(i)
    val flipV = (spriteAttr(i) & 0x80) != 0
    val tile = spriteTile(i)
    val addr = if (spriteHeight == 16) {
      val row = if (flipV) 15 - row0 else row0
      ((tile & 1) << 12) | ((tile & 0xFE) << 4) | (if (row >= 8) 16 else 0) | (row & 7)
    } else {
      val row = if (flipV) 7 - row0 else row0
      (if ((ctrl & 8) != 0) 0x1000 else 0) | (tile << 4) | (row & 7)
    }
    val bits = if (i < spriteCount) {
      val b = read((addr + (if (high) 8 else 0)) & 0x1FFF)
      if ((spriteAttr(i) & 0x40) != 0) reverse(b) else b
    } else { cart.ppuBus(if (spriteHeight == 16) 0x1FF0 else (ctrl & 8) << 9 | 0xFF0, dotCount); 0 }
    if (high) spriteHi(i) = bits else spriteLo(i) = bits
  }
  private def reverse(b: Int) = Integer.reverse(b) >>> 24

  private def pixel(): Unit = {
    val x = dot - 1
    val showBg = (mask & 0x08) != 0 && (x >= 8 || (mask & 0x02) != 0)
    val showSp = (mask & 0x10) != 0 && (x >= 8 || (mask & 0x04) != 0)
    val bit = 15 - fineX
    val bgPix = if (showBg) ((bgLo >> bit) & 1) | (((bgHi >> bit) & 1) << 1) else 0
    val bgPal = if (showBg) ((atLo >> bit) & 1) | (((atHi >> bit) & 1) << 1) else 0
    var spPix, spPal = 0
    var spBehind = false
    var spZero = false
    if (showSp) {
      var i = 0
      while (i < spriteCount && spPix == 0) {
        val off = x - spriteX(i)
        if (off >= 0 && off < 8) {
          val p = ((spriteLo(i) >> (7 - off)) & 1) | (((spriteHi(i) >> (7 - off)) & 1) << 1)
          if (p != 0) {
            spPix = p; spPal = (spriteAttr(i) & 3) | 4; spBehind = (spriteAttr(i) & 0x20) != 0
            spZero = i == 0 && sprite0Line
          }
        }
        i += 1
      }
    }
    if (spZero && bgPix != 0 && x != 255) sprite0Hit = true
    val index = if (bgPix == 0 && spPix == 0) 0
                else if (spPix != 0 && (bgPix == 0 || !spBehind)) spPal << 2 | spPix
                else bgPal << 2 | bgPix
    back(line * 256 + x) = palette(index) & (if ((mask & 1) != 0) 0x30 else 0x3F)
  }

  private def renderingNow = rendering && (line < 240 || line == 261)
  /** One dot. */
  def tick(): Unit = {
    dotCount += 1
    val visible = line < 240
    val pre = line == 261
    if (rendering && (visible || pre)) {
      // The shifters move, then take the next tile every 8 dots, then give this dot's pixel.
      if ((dot >= 2 && dot <= 257) || (dot >= 322 && dot <= 337)) shift()
      if ((dot >= 9 && dot <= 257 || dot == 329 || dot == 337) && ((dot - 1) & 7) == 0) reload()
      if (visible && dot >= 1 && dot <= 256) pixel()
      if ((dot >= 1 && dot <= 256) || (dot >= 321 && dot <= 336)) {
        (dot - 1) & 7 match {
          case 0 => ntByte = read(0x2000 | (v & 0xFFF))
          case 2 =>
            val at = read(0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 7))
            atByte = (at >> (((v >> 4) & 4) | (v & 2))) & 3
          case 4 => patLo = read((if ((ctrl & 0x10) != 0) 0x1000 else 0) | (ntByte << 4) | ((v >> 12) & 7))
          case 6 => patHi = read((if ((ctrl & 0x10) != 0) 0x1000 else 0) | (ntByte << 4) | ((v >> 12) & 7) | 8)
          case 7 => incX()
          case _ =>
        }
      }
      if (visible && dot == 65) evaluate()
      if (visible && dot == overflowDot) overflow = true
      if (dot == 256) incY()
      if (dot == 257) {
        v = (v & ~0x041F) | (t & 0x041F)
        if (!visible) { nextCount = 0; sprite0Next = false }
        loadSprites()
        sprite0Line = sprite0Next
      }
      if (dot >= 257 && dot <= 320 && ((dot - 257) & 5) == 4) fetchSprite((dot - 257) >> 3, ((dot - 257) & 2) != 0)
      // The garbage nametable fetches of the sprite and end-of-line slots (only the bus matters).
      if ((dot >= 257 && dot <= 320 && ((dot - 257) & 5) == 0) || dot == 337 || dot == 339) cart.ppuBus(0x2000 | (v & 0xFFF), dotCount)
      if (pre && dot >= 280 && dot <= 304) v = (v & ~0x7BE0) | (t & 0x7BE0)
    } else if (visible && dot >= 1 && dot <= 256) {
      // Rendering off: the backdrop, or the palette entry v points at.
      val i = if ((v & 0x3F00) == 0x3F00) paletteIndex(v) else 0
      back(line * 256 + dot - 1) = palette(i) & (if ((mask & 1) != 0) 0x30 else 0x3F)
    }
    if (line == 241 && dot == 1) {
      if (!suppress) vblank = true
      suppress = false
      Array.copy(back, 0, frame, 0, back.length)
      frames += 1
      onFrame(frame)
    }
    if (pre && dot == 1) { vblank = false; sprite0Hit = false; overflow = false }

    // Advance; the pre-render line is one dot shorter on odd frames when rendering was on at dot 338.
    if (pre && dot == 338) skipDot = oddFrame && rendering
    if (pre && dot == 339 && skipDot) { dot = 0; line = 0; oddFrame = !oddFrame }
    else {
      dot += 1
      if (dot > 340) {
        dot = 0; line += 1
        if (line > 261) { line = 0; oddFrame = !oddFrame }
      }
    }
  }
}
