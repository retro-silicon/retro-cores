package retro.nes

import retro.core._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

/** One pixel out of the PPU: `color` is a 2C02 palette index (0-63, with the greyscale bit
  * of PPUMASK applied). */
case class NesPixel() extends Bundle {
  val x, y = UInt(8 bits)
  val color = UInt(6 bits)
}

/** A memory write the CPU access makes, which the dot after it must see. */
case class NesPpuWrite[A <: Data, D <: Data](address: A, data: D) {
  val enable = Bool()
}

/** Everything the 2C02 holds between dots, as one bundle so a dot is a function from one value
  * of it to the next (`NesPpuPlugin.dot`), chained three times per CPU cycle. */
case class NesPpuState() extends Bundle {
  val ctrl, mask = UInt(8 bits)
  val vblank, sprite0Hit, overflow, w = Bool()
  val oamAddr = UInt(8 bits)
  val v, t = UInt(15 bits)
  val fineX = UInt(3 bits)
  val buffer, latch = UInt(8 bits)
  /** Frames since each latch bit was last driven, saturating: past 36 the bit has decayed. */
  val age = Vec(UInt(6 bits), 8)
  val dot, line = UInt(9 bits)
  val oddFrame, suppress, skipDot = Bool()
  // Background fetch pipeline.
  val ntByte, patLo, patHi = UInt(8 bits)
  val atByte = UInt(2 bits)
  val bgLo, bgHi, atLo, atHi = UInt(16 bits)
  // The sprites this line draws.
  val spY, spTile, spAttr, spX, spLo, spHi = Vec(UInt(8 bits), 8)
  val spCount = UInt(4 bits)
  val sprite0Line = Bool()
  // Secondary OAM: what evaluation found for the next line.
  val nxY, nxTile, nxAttr, nxX = Vec(UInt(8 bits), 8)
  val nxCount = UInt(4 bits)
  val sprite0Next = Bool()
  // Sprite evaluation in progress: OAM entry n (byte m in the overflow scan) and the dot the
  // hardware's scan would be at.
  val evalActive, evalOverflowScan = Bool()
  val evalN = UInt(7 bits)
  val evalM = UInt(2 bits)
  val evalAt = UInt(10 bits)
  val overflowValid = Bool()
  val overflowDot = UInt(10 bits)
}

/** The NTSC 2C02 PPU, mirroring `NesPpuReference` dot for dot: registers 2000-2007 (mirrored to
  * 3FFF), the loopy v/t/x/w scroll registers, the 2007 read buffer, the decaying data latch,
  * the background shifters, sprite evaluation into secondary OAM with the overflow bug and its
  * timing, sprite pattern fetches in dots 257-320, sprite 0 hit, VBlank and NMI with the 2002
  * read race, and the odd-frame skipped dot. CIRAM (2 KB), OAM and palette RAM live here; CHR
  * and mirroring come from `NesCartridgePlugin`.
  *
  * Three dots per CPU cycle, with the CPU's access between the second and the third, as the
  * model has it. Each dot reads CIRAM, CHR and OAM at most once, through its own port.
  *
  * Where the model takes shortcuts the RTL cannot, the RTL scans OAM one entry per dot from dot
  * 66 instead of all at once at dot 65, with the same result unless OAM or the sprite size
  * changes during the scan. Pixels come out on `pixels`, up to three per clock. */
class NesPpuPlugin extends FiberPlugin {
  var pixels: Vec[Flow[NesPixel]] = null

  /** Power-up palette, as in `NesPpuReference`. */
  val powerUpPalette = Seq(0x09, 0x01, 0x00, 0x01, 0x00, 0x02, 0x02, 0x0D, 0x08, 0x10, 0x08, 0x24, 0x00, 0x00, 0x04, 0x2C,
                           0x09, 0x01, 0x34, 0x03, 0x00, 0x04, 0x00, 0x14, 0x08, 0x3A, 0x00, 0x02, 0x00, 0x20, 0x2C, 0x08)

  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(SizeMapping(0x2000, 0x2000))
    retainer.release()
    val cart = host[NesCartridgePlugin]
    val ints = host[NesInterruptsPlugin]
    pixels = out(Vec.fill(3)(Flow(NesPixel()))).setName("ppu_pixels")

    val state = Reg(NesPpuState())
    for (d <- state.flatten) d match {
      case b: Bool => b.init(False)
      case u: UInt => u.init(0)
    }
    state.line.simPublic()
    state.dot.simPublic()
    val palette = Vec(powerUpPalette.map(c => Reg(UInt(6 bits)).init(c)))
    val ciram = Mem.fill(0x800)(UInt(8 bits)).setName("ciram").simPublic()
    ciram.init(Seq.fill(0x800)(U(0, 8 bits)))
    val oam = Mem.fill(64)(Bits(32 bits)).setName("oam").simPublic()
    oam.init(Seq.fill(64)(B(0, 32 bits)))

    def paletteIndex(a: UInt): UInt = {
      val i = a(4 downto 0)
      (i(4) && i(1 downto 0) === 0) ? (U"0" @@ i(3 downto 0)) | i
    }
    def incX(v: UInt): UInt = (v(4 downto 0) === 31) ? ((v & ~U(0x1F, 15 bits)) ^ U(0x400, 15 bits)) | (v + 1)
    def incY(v: UInt): UInt = {
      val r = UInt(15 bits)
      val y = v(9 downto 5)
      r := v + 0x1000
      when(v(14 downto 12) === 7) {
        r := v
        r(14 downto 12) := 0
        r(9 downto 5) := (y === 29 || y === 31) ? U(0, 5 bits) | (y + 1)
        when(y === 29) { r(11) := !v(11) }
      }
      r
    }

    // Memory writes come from the CPU access in the middle of the cycle; the third dot reads past
    // them, as the model's does.
    val ciramWrite = NesPpuWrite(UInt(11 bits), UInt(8 bits))
    val chrWrite = NesPpuWrite(UInt(13 bits), UInt(8 bits))
    val oamWrite = NesPpuWrite(UInt(6 bits), Bits(32 bits))
    val oamWriteMask = Bits(4 bits)
    val paletteWrite = NesPpuWrite(UInt(5 bits), UInt(6 bits))

    /** One dot. `bypass` makes this dot's memory reads see the access's writes. */
    def dot(s: NesPpuState, pal: Vec[UInt], bypass: Boolean): (NesPpuState, Flow[NesPixel], (Bool, Bool)) = {
      def ciramRead(a: UInt): UInt = {
        val i = cart.ciramIndex(a, afterAccess = bypass)
        val r = ciram.readAsync(i)
        if (bypass) (ciramWrite.enable && ciramWrite.address === i) ? ciramWrite.data | r else r
      }
      def chrRead(a: UInt): UInt = {
        val r = cart.chrRead(a, afterAccess = bypass)
        if (bypass && cart.spec.chrRam) (chrWrite.enable && chrWrite.address === a(12 downto 0)) ? chrWrite.data | r else r
      }
      def oamRead(entry: UInt): Bits = {
        val r = oam.readAsync(entry)
        if (bypass) {
          val merged = Bits(32 bits)
          for (b <- 0 until 4) merged(b * 8 + 7 downto b * 8) := (oamWrite.enable && oamWrite.address === entry && oamWriteMask(b)) ?
            oamWrite.data(b * 8 + 7 downto b * 8) | r(b * 8 + 7 downto b * 8)
          merged
        } else r
      }

      val o = NesPpuState()
      o := s
      val px = Flow(NesPixel())
      val d = s.dot
      val line = s.line
      val visible = line < 240
      val pre = line === 261
      val rendering = s.mask(3) || s.mask(4)
      val renderLine = rendering && (visible || pre)
      val grey = s.mask(0) ? U(0x30, 6 bits) | U(0x3F, 6 bits)
      val h16 = s.ctrl(5)
      def inRange(y: UInt) = line >= y && (line - y) < (h16 ? U(16, 5 bits) | U(8, 5 bits))

      // Shifters move, then take the next tile every 8 dots, then give this dot's pixel.
      val doShift = (d >= 2 && d <= 257) || (d >= 322 && d <= 337)
      val doReload = ((d >= 9 && d <= 257) || d === 329 || d === 337) && (d - 1)(2 downto 0) === 0
      def step(x: UInt, low: UInt) = {
        val shifted = doShift ? (x |<< 1) | x
        doReload ? (shifted(15 downto 8) @@ low) | shifted
      }
      val bgLo = step(s.bgLo, s.patLo)
      val bgHi = step(s.bgHi, s.patHi)
      val atLo = step(s.atLo, s.atByte(0) ? U(0xFF, 8 bits) | U(0, 8 bits))
      val atHi = step(s.atHi, s.atByte(1) ? U(0xFF, 8 bits) | U(0, 8 bits))

      // The pixel.
      val x = (d - 1).resize(8 bits)
      val showBg = s.mask(3) && (x >= 8 || s.mask(1))
      val showSp = s.mask(4) && (x >= 8 || s.mask(2))
      val bit = U(15, 4 bits) - s.fineX.resize(4 bits)
      val bgPix = showBg ? (bgHi(bit) ## bgLo(bit)).asUInt | U(0, 2 bits)
      val bgPal = showBg ? (atHi(bit) ## atLo(bit)).asUInt | U(0, 2 bits)
      val spPixels = (0 until 8).map { i =>
        val off = (x - s.spX(i)).resize(3 bits)
        val col = U(7, 3 bits) - off
        (s.spHi(i)(col) ## s.spLo(i)(col)).asUInt
      }
      val spHits = (0 until 8).map(i => showSp && s.spCount > i && x >= s.spX(i) && (x - s.spX(i)) < 8 && spPixels(i) =/= 0)
      val first = OHMasking.first(spHits.asBits())
      val spPix = spHits.orR ? MuxOH(first, spPixels) | U(0, 2 bits)
      val spAttr = MuxOH(first, s.spAttr)
      val spZero = spHits(0) && s.sprite0Line
      val index = UInt(5 bits)
      index := 0
      when(spPix =/= 0 && (bgPix === 0 || !spAttr(5))) { index := U"1" @@ spAttr(1 downto 0) @@ spPix }
        .elsewhen(bgPix =/= 0) { index := (bgPal @@ bgPix).resized }
      val backdrop = (s.v(13 downto 8) === 0x3F) ? paletteIndex(s.v) | U(0, 5 bits)
      px.valid := visible && d >= 1 && d <= 256
      px.x := x
      px.y := line.resize(8 bits)
      px.color := (renderLine ? pal(index) | pal(backdrop)) & grey

      // Fetches: one CIRAM read and one CHR read at most.
      val fetching = (d >= 1 && d <= 256) || (d >= 321 && d <= 336)
      val phase = (d - 1)(2 downto 0)
      val v = s.v
      val ntAddress = U(0x2, 2 bits) @@ v(11 downto 0)
      val atAddress = U(0x23C, 10 bits) @@ U(0, 4 bits) | (U(0, 2 bits) @@ v(11 downto 10) @@ U(0, 10 bits)) |
        (U(0, 8 bits) @@ v(9 downto 7) @@ U(0, 3 bits)) | (U(0, 11 bits) @@ v(4 downto 2))
      val ciramValue = ciramRead(phase(1) ? atAddress | ntAddress)
      val bgPattern = (s.ctrl(4) ## s.ntByte ## phase(1) ## v(14 downto 12)).asUInt
      val sprite = (d - 257).resize(6 bits)
      val spriteFetch = d >= 257 && d <= 320 && sprite(2) && !sprite(0)
      val si = sprite(5 downto 3)
      val high = sprite(1)
      val row0 = (line - s.spY(si)).resize(4 bits)
      val flipV = s.spAttr(si)(7)
      val tile = s.spTile(si)
      val row = flipV ? ((h16 ? U(15, 4 bits) | U(7, 4 bits)) - row0) | row0
      val spPattern = h16 ? (tile(0) ## tile(7 downto 1) ## row(3) ## high ## row(2 downto 0)).asUInt |
                            (s.ctrl(3) ## tile ## high ## row(2 downto 0)).asUInt
      val chrValue = chrRead(spriteFetch ? spPattern | bgPattern)
      val spBits = (si < s.spCount) ? (s.spAttr(si)(6) ? chrValue.reversed | chrValue) | U(0, 8 bits)
      // The address bus as the cartridge sees it (`NesCartridge.ppuBus`): fetches, sprite
      // pattern fetches (unused slots fetch tile FF), and the garbage nametable fetches.
      val busValid, busHigh = CombInit(False)
      when(renderLine) {
        when(fetching && !phase(0)) { busValid := True; busHigh := phase(2) && s.ctrl(4) }
        when(spriteFetch) { busValid := True; busHigh := h16 ? ((si < s.spCount) ? tile(0) | True) | s.ctrl(3) }
        when((d >= 257 && d <= 320 && !sprite(2) && !sprite(0)) || d === 337 || d === 339) { busValid := True }
      }

      val v1 = (fetching && phase === 7) ? incX(v) | v
      val v2 = (d === 256) ? incY(v1) | v1
      val v3 = (d === 257) ? ((v2 & ~U(0x041F, 15 bits)) | (s.t & U(0x041F, 15 bits))) | v2
      val v4 = (pre && d >= 280 && d <= 304) ? ((v3 & ~U(0x7BE0, 15 bits)) | (s.t & U(0x7BE0, 15 bits))) | v3

      // Sprite evaluation: started at dot 65, then one OAM entry per dot.
      val entry = oamRead(s.evalN(5 downto 0))
      val bytes = entry.subdivideIn(8 bits).map(_.asUInt)
      val slot = s.nxCount.resize(3 bits)

      when(renderLine) {
        o.bgLo := bgLo; o.bgHi := bgHi; o.atLo := atLo; o.atHi := atHi
        when(visible && d >= 1 && d <= 256 && spZero && bgPix =/= 0 && x =/= 255) { o.sprite0Hit := True }
        when(fetching) {
          switch(phase) {
            is(0) { o.ntByte := ciramValue }
            is(2) { o.atByte := (ciramValue >> (v(6) ## v(1) ## False).asUInt).resize(2 bits) }
            is(4) { o.patLo := chrValue }
            is(6) { o.patHi := chrValue }
          }
        }
        o.v := v4
        when(visible && s.overflowValid && d === s.overflowDot) { o.overflow := True }
        when(d === 257) {
          o.spCount := visible ? s.nxCount | U(0, 4 bits)
          o.sprite0Line := visible && s.sprite0Next
          when(!visible) { o.nxCount := 0; o.sprite0Next := False }
          o.spY := s.nxY; o.spTile := s.nxTile; o.spAttr := s.nxAttr; o.spX := s.nxX
        }
        when(spriteFetch) {
          when(high) { o.spHi(si) := spBits } otherwise { o.spLo(si) := spBits }
        }
      }
      when(renderLine && visible && d === 65) {
        o.nxCount := 0
        o.sprite0Next := False
        o.overflowValid := False
        o.evalActive := True
        o.evalOverflowScan := False
        o.evalN := 0
        o.evalM := 0
        o.evalAt := 65
      } elsewhen (s.evalActive) {
        when(!s.evalOverflowScan) {
          val y = bytes(0)
          val found = inRange(y)
          val count = s.nxCount + found.asUInt.resize(4 bits)
          when(found) {
            o.nxY(slot) := y; o.nxTile(slot) := bytes(1); o.nxAttr(slot) := bytes(2); o.nxX(slot) := bytes(3)
            o.nxCount := count
            when(s.evalN === 0) { o.sprite0Next := True }
          }
          o.evalAt := s.evalAt + (found ? U(8, 4 bits) | U(2, 4 bits))
          o.evalN := s.evalN + 1
          when(s.evalN === 63) { o.evalActive := False } elsewhen (count === 8) { o.evalOverflowScan := True }
        } otherwise {
          when(s.evalN >= 64 || s.evalAt > 256) { o.evalActive := False } otherwise {
            when(inRange(bytes(s.evalM))) {
              o.overflowValid := True
              o.overflowDot := s.evalAt + 1
              o.evalActive := False
            } otherwise {
              o.evalN := s.evalN + 1
              o.evalM := s.evalM + 1
              o.evalAt := s.evalAt + 2
            }
          }
        }
      }

      when(line === 241 && d === 1) {
        when(!s.suppress) { o.vblank := True }
        o.suppress := False
        for (b <- 0 until 8) o.age(b) := (s.age(b) === 63) ? s.age(b) | (s.age(b) + 1)
      }
      when(pre && d === 1) { o.vblank := False; o.sprite0Hit := False; o.overflow := False }

      // Advance; the pre-render line is one dot shorter on odd frames when rendering was on at
      // dot 338.
      when(pre && d === 338) { o.skipDot := s.oddFrame && rendering }
      when(pre && d === 339 && s.skipDot) {
        o.dot := 0; o.line := 0; o.oddFrame := !s.oddFrame
      } elsewhen (d === 340) {
        o.dot := 0
        o.line := pre ? U(0, 9 bits) | (line + 1)
        when(pre) { o.oddFrame := !s.oddFrame }
      } otherwise { o.dot := d + 1 }
      (o, px, (busValid, busHigh))
    }

    // ---- The CPU access ----------------------------------------------------------------------
    def access(s: NesPpuState): (NesPpuState, Seq[(Bool, Bool)]) = {
      val o = NesPpuState()
      o := s
      val register = slave.cmd.address(2 downto 0)
      val value = slave.cmd.data
      val decayed = (0 until 8).map(b => s.latch(b) && s.age(b) <= 36).asBits().asUInt
      def drive(data: UInt, mask: Int): UInt = {
        val r = (data & U(mask, 8 bits)) | (decayed & U(~mask & 0xFF, 8 bits))
        o.latch := r
        for (b <- 0 until 8 if (mask >> b & 1) != 0) o.age(b) := 0
        r
      }
      val a = s.v(13 downto 0)
      val toPalette = a >= 0x3F00
      val ciramValue = ciram.readAsync(cart.ciramIndex(a))
      val chrValue = cart.chrRead(a)
      val oamEntry = oam.readAsync(s.oamAddr(7 downto 2))
      val oamByte = oamEntry.subdivideIn(8 bits)(s.oamAddr(1 downto 0)).asUInt
      val increment = s.ctrl(2) ? U(32, 15 bits) | U(1, 15 bits)

      ciramWrite.address := cart.ciramIndex(a)
      ciramWrite.data := value
      ciramWrite.enable := False
      chrWrite.address := a(12 downto 0)
      chrWrite.data := value
      chrWrite.enable := False
      oamWrite.address := s.oamAddr(7 downto 2)
      oamWrite.data := ((s.oamAddr(1 downto 0) === 2) ? (value & U(0xE3, 8 bits)) | value).asBits #* 4
      oamWriteMask := UIntToOh(s.oamAddr(1 downto 0), 4)
      oamWrite.enable := False
      paletteWrite.address := paletteIndex(a)
      paletteWrite.data := value(5 downto 0)
      paletteWrite.enable := False

      // What a read returns; latch refreshes and side effects only when it completes.
      val status = (s.vblank ## s.sprite0Hit ## s.overflow ## B"00000").asUInt
      val paletteValue = (U"00" @@ (palette(paletteIndex(a)) & (s.mask(0) ? U(0x30, 6 bits) | U(0x3F, 6 bits))))
      val readValue = UInt(8 bits)
      readValue := decayed
      switch(register) {
        is(2) { readValue := (status & U(0xE0, 8 bits)) | (decayed & U(0x1F, 8 bits)) }
        is(4) { readValue := oamByte }
        is(7) { readValue := toPalette ? ((paletteValue & U(0x3F, 8 bits)) | (decayed & U(0xC0, 8 bits))) | s.buffer }
      }
      slave.rdata := readValue

      when(slave.readFire) {
        switch(register) {
          is(2) {
            drive(status, 0xE0)
            o.vblank := False
            o.w := False
            when(s.line === 241 && s.dot === 1) { o.suppress := True }
          }
          is(4) { drive(oamByte, 0xFF) }
          is(7) {
            when(toPalette) {
              drive(paletteValue, 0x3F)
              o.buffer := ciramValue
            } otherwise {
              drive(s.buffer, 0xFF)
              o.buffer := (a < 0x2000) ? chrValue | ciramValue
            }
            o.v := s.v + increment
          }
        }
      }
      when(slave.writeFire) {
        drive(value, 0xFF)
        switch(register) {
          is(0) { o.ctrl := value; o.t(11 downto 10) := value(1 downto 0) }
          is(1) { o.mask := value }
          is(3) { o.oamAddr := value }
          is(4) { oamWrite.enable := True; o.oamAddr := s.oamAddr + 1 }
          is(5) {
            when(!s.w) { o.t(4 downto 0) := value(7 downto 3); o.fineX := value(2 downto 0) }
              .otherwise { o.t(14 downto 12) := value(2 downto 0); o.t(9 downto 5) := value(7 downto 3) }
            o.w := !s.w
          }
          is(6) {
            when(!s.w) { o.t(14) := False; o.t(13 downto 8) := value(5 downto 0) }
              .otherwise { o.t(7 downto 0) := value; o.v := (s.t(14 downto 8) @@ value) }
            o.w := !s.w
          }
          is(7) {
            when(a < 0x2000) { chrWrite.enable := True }
              .elsewhen(!toPalette) { ciramWrite.enable := True }
              .otherwise { paletteWrite.enable := True }
            o.v := s.v + increment
          }
        }
      }
      // The bus events the model's `ppuBus` sees: the 2007 access's address (none for palette
      // reads), then v after a 2006 or 2007 access while not rendering.
      val renderingNow = (s.mask(3) || s.mask(4)) && (s.line < 240 || s.line === 261)
      val data7 = register === 7 && (slave.readFire && !toPalette || slave.writeFire)
      val vChanged = (register === 7 && (slave.readFire || slave.writeFire)) || (register === 6 && slave.writeFire && s.w)
      (o, Seq((data7, a(12)), (vChanged && !renderingNow, o.v(12))))
    }

    // ---- Three dots and the access in between -------------------------------------------------
    val paletteAfter = CombInit(palette)
    when(paletteWrite.enable) { paletteAfter(paletteWrite.address) := paletteWrite.data }
    val (s1, p1, b1) = dot(state, palette, bypass = false)
    val (s2, p2, b2) = dot(s1, palette, bypass = false)
    val (s2a, accessBus) = access(s2)
    val (s3, p3, b3) = dot(s2a, paletteAfter, bypass = true)
    cart.ppuBus(Seq(b1, b2, b3), accessBus)
    state := s3
    palette := paletteAfter
    pixels(0) := p1; pixels(1) := p2; pixels(2) := p3
    /** The three pixels as one value (valid, y, x, color from the MSB; pixel 0 lowest), so a
      * sim reads them in one go. */
    val pixelTap = pixels.map(p => p.valid ## p.y ## p.x ## p.color).reduce((low, high) => high ## low).setName("ppu_pixel_tap").simPublic()
    ciram.write(ciramWrite.address, ciramWrite.data, ciramWrite.enable)
    cart.chrWrite(chrWrite.address, chrWrite.data, chrWrite.enable)
    oam.write(oamWrite.address, oamWrite.data, oamWrite.enable, oamWriteMask)
    ints.nmi := s3.vblank && s3.ctrl(7)
  }
}
