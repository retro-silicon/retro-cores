package retro.gb

import retro.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._

/** Up to four pixels a cycle (one per dot) for the LCD: `valid(i)` marks the pixels sent, `x(i)`
  * their columns on line `y`, `shade(i)` their shades, 0 (white) to 3 (black). `fill` ends a line
  * that sent fewer than 160 pixels (the window can draw a column twice): columns `fillX` to 159
  * take `fillShade`. */
case class LcdPixels() extends Bundle {
  val valid = Bits(4 bits)
  val x = Vec(UInt(8 bits), 4)
  val y = UInt(8 bits)
  val shade = Vec(UInt(2 bits), 4)
  val fill = Bool()
  val fillX = UInt(8 bits)
  val fillShade = UInt(2 bits)
}

object LcdPixels {
  /** For sims: draws the pixels on `lcd` this cycle into `frame` (160 x 144, row-major). */
  def capture(lcd: LcdPixels, frame: Array[Int]): Unit = {
    val valid = lcd.valid.toInt
    val y = lcd.y.toInt
    if (valid != 0) for (i <- 0 until 4 if (valid >> i & 1) != 0) frame(y * 160 + lcd.x(i).toInt) = lcd.shade(i).toInt
    if (lcd.fill.toBoolean) for (x <- lcd.fillX.toInt until 160) frame(y * 160 + x) = lcd.fillShade.toInt
  }
}

/** The pixel transfer state that changes dot by dot: SameBoy's DMG FIFO and fetcher, as
  * `PpuReference` describes them; the names follow it. */
case class PpuDot() extends Bundle {
  /** Mode 3 as the CPU sees it, and the transfer running. */
  val transfer, active = Bool()
  /** `PpuReference.sbStep`'s state, and dots to wait before its next step. */
  val st = UInt(4 bits)
  val sleep = UInt(3 bits)
  val pos = SInt(9 bits)
  val lcdX = UInt(8 bits)
  // FIFOs: entry 0 leaves first.
  val bg = Vec(UInt(2 bits), 8)
  val bgSize = UInt(4 bits)
  val obPix = Vec(UInt(2 bits), 8)
  val obPal, obPrio = Bits(8 bits)
  val obSize = UInt(4 bits)
  // Background fetcher
  val fSt = UInt(3 bits)
  val tileIdxAddr, tileDataAddr = UInt(13 bits)
  val curTile, data0, data1 = UInt(8 bits)
  val wxTriggered, winFetching, insertBg, noInsertGlitch, duringObjFetch, abortObj = Bool()
  val winTileX = UInt(5 bits)
  val winY = UInt(8 bits)
  // Objects
  val objDone = Bits(10 bits)
  val slot = UInt(4 bits)
  val oTile, oFlags, objLo = UInt(8 bits)
  val lastShade = UInt(2 bits)
}

/** The DMG PPU: LCDC, STAT, SCY, SCX, LY, LYC, BGP, OBP0, OBP1, WY, WX, the VBlank and STAT
  * interrupts, and the pixels, sent out on `lcd`. It reads VRAM and OAM through their own ports
  * (`RamPlugin.directRead`, `OamDmaPlugin.oamRead`), not over the CPU's bus.
  *
  * The design runs one clock per M-cycle and the PPU works in dots, four per M-cycle, so each
  * clock chains four copies of the one-dot step (`dot`), each with its own VRAM read port; the
  * OAM scan checks two OAM entries a clock. Mode 3 ends for the CPU 3 dots before the transfer
  * does, so three more copies look ahead, without VRAM. A CPU write to a PPU register lands
  * between the dots (`PpuReference.writeDot`). The timing, and what it leaves out, is described
  * at `PpuReference`, the model the system cosim checks this against dot for dot. */
class PpuPlugin(postBoot: Boolean = false)
    extends GbPeripheral(SizeMapping(Gb.LCDC, 0x0C), Gb.Irq.VBLANK, Gb.Irq.STAT) {
  var lcd: LcdPixels = null
  /** LCDC, for a sim to tell whether the LCD is on. */
  var lcdcReg: UInt = null
  private var statLateHalted: Bool = null
  override def withController(ctrl: IrqService): Unit = ctrl match {
    case c: InterruptControllerPlugin =>
      statLateHalted = c.late(Gb.Irq.STAT, halted = true)
    case _ =>
  }

  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    lcd = out(LcdPixels()).setName("lcd")
    val vram = host.list[RamPlugin].find(_.ramName == "vram").get
    val dma = host[OamDmaPlugin]

    def boot(after: Int, before: Int = 0) = if (postBoot) after else before
    val lcdc = Reg(UInt(8 bits)).init(boot(Gb.PostBoot.lcdc)).simPublic()
    lcdcReg = lcdc
    val statSelect = Reg(UInt(4 bits)) init 0
    val scy, scx, lyc, wy, wx = Reg(UInt(8 bits)) init 0
    val bgp = Reg(UInt(8 bits)) init boot(Gb.PostBoot.bgp)
    val obp0, obp1 = Reg(UInt(8 bits)) init boot(Gb.PostBoot.obp)
    val x = Reg(UInt(7 bits)).init(boot(Gb.PostBoot.ppuX)).simPublic()
    val ly = Reg(UInt(8 bits)).init(boot(Gb.PostBoot.ppuLy)).simPublic()
    val statLine = RegInit(False)
    val coincidence = RegInit(False)
    // The LY = LYC interrupt line: while LY is not compared (the flag reads 0) it holds its value.
    val lycLine = RegInit(False)
    val firstLine = RegInit(False)
    /** The last M-cycle's mode for the STAT interrupt. */
    val irqModeLast = Reg(UInt(2 bits)) init 0
    val on = lcdc(7)

    val objY, objX = Vec.fill(10)(Reg(UInt(8 bits)) init 0)
    val objIndex = Vec.fill(10)(Reg(UInt(6 bits)) init 0)
    val objCount = Reg(UInt(4 bits)) init 0
    val wyHit = RegInit(False)
    val st = Reg(PpuDot())
    st.transfer init False
    st.active init False
    st.winY init 0xFF

    // ---- The tick: line and frame sequencing ---------------------------------------------
    val vblank = on && ly === Gb.visibleLines - 1 && x === Gb.lineCycles - 1
    val lineStart = !on || x === Gb.lineCycles - 1
    val xTicked = UInt(7 bits)
    val lyTicked = UInt(8 bits)
    when(!on) {
      xTicked := 0
      lyTicked := 0
    } elsewhen (x === Gb.lineCycles - 1) {
      xTicked := 0
      lyTicked := (ly === Gb.frameLines - 1) ? U(0, 8 bits) | ly + 1
    } otherwise {
      xTicked := x + 1
      lyTicked := ly
    }
    val visible = on && lyTicked < Gb.visibleLines
    val firstLineTicked = !on || (firstLine && !lineStart)
    /** The pixel transfer's part of the line, where the dots run. */
    val transferPart = visible && xTicked >= Gb.oamEnd

    // Line start, then the window's line trigger and the OAM scan.
    val s0 = PpuDot()
    s0 := st
    val wyHitTicked = Bool()
    val countScanned = UInt(4 bits)
    wyHitTicked := wyHit
    countScanned := objCount
    when(lineStart) {
      when(lyTicked === 0) {
        wyHitTicked := False
        s0.winY := 0xFF
      }
      s0.transfer := False
      s0.active := False
      countScanned := 0
    }
    when(visible && lyTicked === wy && lcdc(5)) { wyHitTicked := True }

    val height = lcdc(2) ? U(16, 8 bits) | U(8, 8 bits)
    val scan = for (j <- 0 until 2) yield new Area {
      val entry = (xTicked(4 downto 0) @@ U(j, 1 bits)).resize(6)
      def byte(k: Int) = dma.oamRead((entry @@ U(k, 2 bits)).resize(8))
      val y = byte(0)
      val line = lyTicked + 16
      val hit = visible && xTicked < Gb.oamEnd && line >= y && line < y + height
    }
    // Slots are filled in OAM order; the second entry of the clock goes after the first.
    val slot0 = countScanned
    val take0 = scan(0).hit && slot0 < 10
    val slot1 = slot0 + take0.asUInt
    val take1 = scan(1).hit && slot1 < 10
    for ((take, slot, j) <- Seq((take0, slot0, 0), (take1, slot1, 1))) when(take) {
      objY(slot.resized) := scan(j).byte(0)
      objX(slot.resized) := scan(j).byte(1)
      objIndex(slot.resized) := scan(j).entry
    }
    objCount := slot1 + take1.asUInt

    when(visible && xTicked === Gb.oamEnd) {
      s0.transfer := True
      s0.active := True
      s0.st := 0
      s0.sleep := 7
      s0.pos := -16
      s0.lcdX := 0
      for (k <- 0 until 8) s0.bg(k) := 0
      s0.bgSize := 8
      s0.obSize := 0
      s0.fSt := 0
      s0.insertBg := False
      s0.noInsertGlitch := False
      s0.wxTriggered := False
      s0.abortObj := False
      s0.duringObjFetch := False
      s0.objDone := 0
    }

    // ---- The pixel transfer: one dot -----------------------------------------------------
    // The CPU's write this M-cycle, or the one before to a register the transfer reads
    // (`PpuReference.lateWrite`): it lands between the dots.
    def lateWrite(a: UInt, d: UInt) = Seq(Gb.SCY, Gb.SCX, Gb.BGP, Gb.OBP0, Gb.OBP1, Gb.WX).map(a === _).orR ||
      a === Gb.LCDC && ((lcdc ^ d) & 0x84) === 0
    val cpuLate = slave.writeFire && lateWrite(slave.cmd.address, slave.cmd.data)
    val held = RegNext(cpuLate) init False
    val heldAddress = RegNextWhen(slave.cmd.address, cpuLate)
    val heldData = RegNextWhen(slave.cmd.data, cpuLate)
    val write = held || slave.writeFire && !cpuLate
    val wAddress = held ? heldAddress | slave.cmd.address
    val wData = held ? heldData | slave.cmd.data
    def writes(a: Int) = write && wAddress === a
    /** LCDC writes that keep the LCD on or off land between the dots; others after them. */
    val lcdcWrite = writes(Gb.LCDC) && wData(7) === lcdc(7)
    /** A register as dot `i` sees it, the write landing before dot `landsAt`. */
    def seen(reg: UInt, a: Int, landsAt: Int, i: Int) = if (i >= landsAt) writes(a) ? wData | reg else reg
    /** A palette: it shows old | new for the dot before the write lands. */
    def palette(reg: UInt, a: Int, i: Int) = if (i == 1) writes(a) ? (reg | wData) | reg else seen(reg, a, 2, i)

    def shift[T <: Data](v: Vec[T], zero: T) = Vec((1 until 8).map(v(_)) :+ zero)

    /** Dot `i` of the M-cycle (5 to 7: the look-ahead, with no VRAM, no pixels and no writes
      * landing). `lcdcIn` is LCDC as the dot before left it, which a write can glitch. */
    def dot(s: PpuDot, lcdcIn: UInt, i: Int) = new Area {
      val dry = i > 4
      val n = PpuDot().allowOverride()
      val lcdcOut = UInt(8 bits)
      val pixel = Flow(UInt(2 bits))
      val pixelX = UInt(8 bits)
      val fill = Bool()
      pixel.valid := False
      pixel.payload := 0
      fill := False

      val scxD = seen(scx, Gb.SCX, 1, i)
      val scyD = seen(scy, Gb.SCY, 2, i)
      val wxD = seen(wx, Gb.WX, 3, i)
      val bgpD = palette(bgp, Gb.BGP, i)
      val obp0D = palette(obp0, Gb.OBP0, i)
      val obp1D = palette(obp1, Gb.OBP1, i)
      val wxJustChanged = if (i == 3) writes(Gb.WX) else False

      // LCDC: the write lands before dot 2, aborting an object fetch if it hides objects; before
      // that it shows old | new BG_EN, with OBJ_EN cleared at X 0 or during an object fetch.
      val s1 = PpuDot()
      s1 := s
      val lcdcD = UInt(8 bits)
      lcdcD := lcdcIn
      if (i == 2) when(lcdcWrite) {
        when(lcdcIn(1) && !wData(1) && s.duringObjFetch) { s1.abortObj := True }
        lcdcD := wData
      }
      if (i == 1) when(lcdcWrite) {
        val old = UInt(8 bits)
        old := lcdcIn
        when((s.pos === 0 || s.duringObjFetch) && !wData(1)) {
          when(lcdcIn(1) && s.duringObjFetch) { s1.abortObj := True }
          old(1) := False
        }
        when(lcdcIn(5) && !wData(5) && s.winFetching) { s1.noInsertGlitch := True }
        lcdcD := old | (wData & 1)
      }
      lcdcOut := lcdcD

      def objHits(t: PpuDot, at: UInt) = (0 until 10).map(k => k < objCount && !t.objDone(k) && objX(k) === at)
      def fetcherY(t: PpuDot) = t.wxTriggered ? t.winY | (lyTicked + scyD)

      // An object fetch that `abortObj` cut short goes on to the pixel step.
      val s2 = PpuDot()
      s2 := s1
      when(s1.abortObj) {
        s2.abortObj := False
        when(s1.st >= 1 && s1.st <= 6) { s2.st := 7; s2.sleep := 0 }
      }
      val st0 = s2.st
      val stepping = s2.sleep === 0

      // State 0: the window check.
      val win = new Area {
        val a = PpuDot()
        a := s2
        val p7 = (s2.pos + 7).asUInt.resize(8)
        val p6 = (s2.pos + 6).asUInt.resize(8)
        val check = st0 === 0 && !s2.wxTriggered && wyHitTicked && lcdcD(5)
        val m7 = wxD === p7
        val m6 = wxD === p6 && !wxJustChanged
        val atZero = s2.pos === -7 || (s2.pos === -16 && scxD(2 downto 0) =/= 0) || (s2.pos >= -15 && s2.pos <= -8)
        val act = (wxD === 0) ? atZero | (wxD < 166 && (m7 || m6))
        when(check && act) {
          a.winY := s2.winY + 1
          a.winTileX := 0
          a.bgSize := 0
          a.wxTriggered := True
          a.fSt := 0
          a.winFetching := True
          when(wxD =/= 0 && !m7 && s2.lcdX =/= 0) { a.lcdX := s2.lcdX - 1 }
        } elsewhen (check && wxD === 166 && m7) {
          a.winY := s2.winY + 1
        }
        val delay = check && act && wxD === 0 && scxD(2 downto 0) =/= 0
      }

      // State 9: the window's background insertion, then objects passed and the next state.
      val goes9 = (st0 === 0 && !win.delay) || st0 === 8
      val b = PpuDot()
      b := win.a
      val xMatch = (win.a.pos + 8 < 0) ? U(0, 9 bits) | (win.a.pos + 8).asUInt
      when(goes9) {
        when(wxD === win.p7 && win.a.wxTriggered && !win.a.winFetching && win.a.fSt === 0 && win.a.bgSize === 8) { b.insertBg := True }
        b.objDone := win.a.objDone | (0 until 10).map(k => k < objCount && objX(k) < xMatch).asBits()
        b.duringObjFetch := True
      }
      val hits = objHits(b, xMatch.resize(8)).asBits()
      val pending = lcdcD(1) && hits =/= 0
      val st1 = UInt(4 bits)
      st1 := st0
      when(goes9 || st0 === 6) { st1 := pending ? U(1, 4 bits) | U(7, 4 bits) }

      // State 7: one pixel out of the FIFOs.
      val render = new Area {
        val r = PpuDot()
        r := b
        val emit = Bool()
        val shade = UInt(2 bits)
        emit := False
        shade := 0
        val waitObj = lcdcD(1) && objHits(b, U(0, 8 bits)).orR
        when(!waitObj && b.bgSize =/= 0) {
          val bgPixel = b.insertBg ? U(0, 2 bits) | b.bg(0)
          when(b.insertBg) { r.insertBg := False } otherwise {
            r.bg := shift(b.bg, U(0, 2 bits))
            r.bgSize := b.bgSize - 1
          }
          val hasObj = b.obSize =/= 0
          val objPixel = hasObj ? b.obPix(0) | U(0, 2 bits)
          when(hasObj) {
            r.obPix := shift(b.obPix, U(0, 2 bits))
            r.obPal := False ## b.obPal(7 downto 1)
            r.obPrio := False ## b.obPrio(7 downto 1)
            r.obSize := b.obSize - 1
          }
          val left = b.pos >= -16 && b.pos < -8
          val low = b.pos.asBits(2 downto 0).asUInt
          val toM8 = left && (low === scxD(2 downto 0) || (b.winFetching && low === 6 && scxD(2 downto 0) === 7))
          val posA = toM8 ? S(-8, 9 bits) | b.pos
          when(left && !toM8 && b.pos === -9) {
            r.pos := -16
          } otherwise {
            r.winFetching := False
            r.pos := posA + 1
            when(posA >= 0 && posA < 160) {
              val c = lcdcD(0) ? bgPixel | U(0, 2 bits)
              val draw = objPixel =/= 0 && lcdcD(1) && !(c =/= 0 && b.obPrio(0))
              val pal = draw ? (b.obPal(0) ? obp1D | obp0D) | bgpD
              shade := pal.subdivideIn(2 bits)((draw ? objPixel | c))
              emit := b.lcdX < 160
              r.lastShade := shade
              r.lcdX := b.lcdX + 1
            }
          }
        }
      }

      // The fetcher steps in states 1, 3 and 7, after the pixel in 7; it reads VRAM at its
      // steps 1, 3 and 5, and the object fetch at states 4 and 5.
      val pre = PpuDot()
      pre := b
      when(st1 === 7) { pre := render.r }
      val objRow = new Area {
        val tall = lcdcD(2)
        val mask = tall ? U(15, 4 bits) | U(7, 4 bits)
        val y = (lyTicked - objY(b.slot.resized)).resize(4) & mask
        val row = b.oFlags(6) ? (y ^ mask) | y
        val tile = b.oTile & (tall ? U(0xFE, 8 bits) | U(0xFF, 8 bits))
        def address(high: Int) = ((tile @@ U(0, 4 bits)) + (row @@ U(high, 1 bits)).resized).resize(13)
      }
      val vAddr = UInt(13 bits)
      vAddr := (b.fSt === 1) ? b.tileIdxAddr | b.tileDataAddr
      when(st1 === 4) { vAddr := objRow.address(0) }
      when(st1 === 5) { vAddr := objRow.address(1) }
      val vData = if (dry) U(0, 8 bits) else vram.directRead(vAddr)

      val adv = PpuDot()
      adv := pre
      val push = new Area {
        val data1 = (pre.fSt === 5) ? vData | pre.data1
        val p7 = pre.pos.resize(10) + 7
        val insert = wyHitTicked && !lcdcD(5) && !pre.noInsertGlitch && p7 >= 0 && wxD === p7.asUInt.resize(8)
        def apply(): Unit = {
          adv.fSt := 6
          when(pre.bgSize === 0) {
            when(insert) {
              adv.bg(0) := 0
              adv.bgSize := 1
            } otherwise {
              for (k <- 0 until 8) adv.bg(k) := (data1(7 - k) ## pre.data0(7 - k)).asUInt
              adv.bgSize := 8
              adv.fSt := 0
            }
          }
        }
      }
      switch(pre.fSt) {
        is(0) {
          val wxT = pre.wxTriggered && lcdcD(5)
          adv.wxTriggered := wxT
          val map = ((!wxT && lcdcD(3)) || (wxT && lcdcD(6))) ? U(0x1C00, 13 bits) | U(0x1800, 13 bits)
          val y = wxT ? pre.winY | (lyTicked + scyD)
          val scrolled = (scxD.resize(10) + (pre.pos + 8).asUInt.resize(10))(7 downto 3)
          val col = wxT ? pre.winTileX | ((pre.pos < -8) ? scxD(7 downto 3) | scrolled)
          adv.tileIdxAddr := map + (y(7 downto 3) @@ col).resized
          adv.fSt := 1
        }
        is(1) { adv.curTile := vData; adv.fSt := 2 }
        is(2, 4) {
          val upper = !lcdcD(4) && !pre.curTile(7)
          adv.tileDataAddr := (upper.asUInt @@ pre.curTile @@ fetcherY(pre)(2 downto 0) @@ (pre.fSt === 4).asUInt)
          adv.fSt := pre.fSt + 1
        }
        is(3) { adv.data0 := vData; adv.fSt := 4 }
        is(5) {
          adv.data1 := vData
          when(pre.wxTriggered) { adv.winTileX := pre.winTileX + 1 }
          push()
        }
        default { push() }
      }

      pixelX := b.lcdX
      n := s2
      when(!transferPart) {
        n := s
      } elsewhen (!s.active) {
        n := s1
      } elsewhen (!stepping) {
        n.sleep := s2.sleep - 1
      } elsewhen (st0 === 0 && win.delay) {
        n := win.a
        n.st := 8
        n.sleep := 0
      } elsewhen (st1 === 1) {
        n := adv
        n.st := (b.fSt < 5 || b.bgSize === 0) ? U(1, 4 bits) | U(3, 4 bits)
        n.sleep := 0
      } elsewhen (st1 === 3) {
        n := adv
        val slot = OHToUInt(OHMasking.first(hits))
        val entry = objIndex(slot.resized)
        n.slot := slot.resized
        n.oTile := (if (dry) U(0, 8 bits) else dma.oamRead((entry @@ U(2, 2 bits)).resize(8)))
        n.oFlags := (if (dry) U(0, 8 bits) else dma.oamRead((entry @@ U(3, 2 bits)).resize(8)))
        n.st := 4
        n.sleep := 1
      } elsewhen (st1 === 4) {
        n := b
        n.objLo := vData
        n.st := 5
        n.sleep := 1
      } elsewhen (st1 === 5) {
        n := b
        n.duringObjFetch := False
        // Mix the object's row into the object FIFO where that is transparent.
        val flip = b.oFlags(5)
        for (k <- 0 until 8) {
          val keep = k < b.obSize
          val pix = keep ? b.obPix(k) | U(0, 2 bits)
          val p = flip ? (vData(k) ## b.objLo(k)).asUInt | (vData(7 - k) ## b.objLo(7 - k)).asUInt
          val set = p =/= 0 && pix === 0
          n.obPix(k) := set ? p | pix
          n.obPal(k) := set ? b.oFlags(4) | (keep && b.obPal(k))
          n.obPrio(k) := set ? b.oFlags(7) | (keep && b.obPrio(k))
        }
        n.obSize := 8
        n.objDone(b.slot.resized) := True
        n.st := 6
        n.sleep := 0
      } otherwise { // 7
        n := adv
        n.duringObjFetch := False
        pixel.valid := (if (dry) False else render.emit)
        pixel.payload := render.shade
        when(render.r.pos === 160) {
          n.active := False
          n.wxTriggered := False
          fill := (if (dry) False else render.r.lcdX < 160)
        } otherwise {
          n.st := 0
          n.sleep := 0
        }
      }
    }.setCompositeName(this, s"dot$i")

    val dots = scala.collection.mutable.ArrayBuffer[Area { val n: PpuDot; val lcdcOut: UInt; val pixel: Flow[UInt]; val pixelX: UInt; val fill: Bool }]()
    for (i <- 1 to 7) dots += dot(if (i == 1) s0 else dots(i - 2).n, if (i == 1) lcdc else dots(i - 2).lcdcOut, i)
    // Mode 3 as the CPU sees it after dot `i`: the transfer has not ended 3 dots later.
    def transferAfter(i: Int) = s0.transfer && dots(i + 2).n.active
    st.allowOverride()
    st := dots(3).n
    st.transfer := transferPart ? transferAfter(4) | s0.transfer
    for (i <- 0 until 4) {
      lcd.valid(i) := dots(i).pixel.valid
      lcd.x(i) := dots(i).pixelX
      lcd.shade(i) := dots(i).pixel.payload
    }
    lcd.y := lyTicked
    lcd.fill := (0 until 4).map(dots(_).fill).orR
    lcd.fillX := dots(3).n.lcdX
    lcd.fillShade := dots(3).n.lastShade

    // STAT reads the mode after the third dot, the STAT interrupt after the second (before the
    // dots in the transfer's first M-cycle). The first line after the LCD is turned on has no
    // mode 2.
    val scanMode = firstLineTicked ? U(0, 2 bits) | U(2, 2 bits)
    // The first line's mode 0 before its transfer is no STAT interrupt source, and its mode 0
    // interrupt sees the transfer as it was before the dots.
    val scanIrqMode = firstLineTicked ? U(3, 2 bits) | U(2, 2 bits)
    val mode, irqMode, irqModeBefore = UInt(2 bits)
    mode := 0
    irqMode := 0
    when(on) {
      when(lyTicked >= Gb.visibleLines) {
        // The last M-cycle of the frame reads as mode 0.
        mode := (lyTicked === Gb.frameLines - 1 && xTicked === Gb.lineCycles - 1) ? U(0, 2 bits) | U(1, 2 bits)
        irqMode := 1
      } elsewhen (xTicked < Gb.oamEnd) {
        mode := scanMode
        irqMode := scanIrqMode
      } otherwise {
        when(transferAfter(3)) { mode := 3 }
        when(xTicked === Gb.oamEnd) { irqMode := scanIrqMode } elsewhen (firstLineTicked ? s0.transfer | transferAfter(2)) { irqMode := 3 }
      }
      // The next line's mode 2 interrupt rises with LY, except line 0's.
      when(xTicked === Gb.lineCycles - 1 && lyTicked < Gb.visibleLines - 1) { irqMode := 2 }
    }
    irqModeBefore := irqMode
    when(transferPart) { irqModeBefore := (xTicked === Gb.oamEnd) ? scanIrqMode | (s0.transfer ? U(3, 2 bits) | U(0, 2 bits)) }
    // LY reads 0 from line 153's second M-cycle.
    val lyNow = (lyTicked === Gb.frameLines - 1 && xTicked >= 1) ? U(0, 8 bits) | lyTicked
    // LY = LYC is only compared while the LCD is on, and stays on the interrupt line when off.
    // It drops in the line's last M-cycle, where LY already reads the next line (except at the
    // end of the frame, where LY stays 0), and in line 153's second, where LY changes to 0.
    val compared = (xTicked =/= Gb.lineCycles - 1 || lyTicked === Gb.frameLines - 1) && !(lyTicked === Gb.frameLines - 1 && xTicked === 1)
    val coincidenceTicked = on ? (compared && lyNow === lyc) | coincidence
    val lycLineTicked = (on && compared) ? (lyNow === lyc) | lycLine
    // Entering VBlank also counts as the start of a mode 2 for the STAT interrupt.
    val vblankMode2 = lyTicked === Gb.visibleLines && xTicked === 0
    def lineFor(m: UInt, lycHigh: Bool = lycLineTicked) = (statSelect(3) && lycHigh) || on && ((statSelect(2) && (m === 2 || vblankMode2)) ||
      (statSelect(1) && m === 1) || (statSelect(0) && m === 0))
    val line = lineFor(irqMode)
    // LY reads the next line in the last M-cycle of a line.
    val lyRead = (on && xTicked === Gb.lineCycles - 1) ? ((lyTicked === Gb.frameLines - 1) ? U(0, 8 bits) | lyTicked + 1) | lyNow
    // The held LY = LYC line still counts at this M-cycle's first dot, against a STAT just written;
    // entering VBlank, LY = LYC is compared before the mode changes.
    // A STAT write on the DMG first enables every STAT interrupt source for a dot, as things stood
    // after the last M-cycle; see `PpuReference`.
    val statWrite = slave.writeFire && slave.cmd.address === Gb.STAT
    val glitchMode2 = !(irqModeLast === 2 && x === Gb.lineCycles - 1 && statSelect(0) && !statSelect(2))
    val glitchModeOff = irqModeLast === 2 && x =/= Gb.lineCycles - 1 && !(x === 0 && ly === 0) ||
      firstLine && x <= Gb.oamEnd || ly === Gb.frameLines - 1 && x === Gb.lineCycles - 1
    val writeRise = statWrite && on && (lycLine || !glitchModeOff && irqModeLast =/= 3 && (irqModeLast =/= 2 || glitchMode2))
    val start = statLine || statSelect(3) && lycLine || writeRise
    val mid = vblankMode2 ? lineFor(0) | line
    val stat = !statLine && start || !start && mid || !mid && line
    irq(0) := vblank
    irq(1) := stat
    // A halted CPU sees the STAT line as it was before the dots.
    if (statLateHalted != null) statLateHalted := stat && !(writeRise && !statLine) && !lineFor(irqModeBefore, lycLineTicked || lycLine)

    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.read(lcdc, Gb.LCDC)
    regs.readAndWrite(statSelect, Gb.STAT, 3)
    // The CPU's VRAM and OAM accesses the PPU blocks; see `PpuReference.vramReadLocked`.
    val lastOfScan = mode === 2 && xTicked === Gb.oamEnd - 1
    vram.readLock.load(mode === 3 || lastOfScan)
    vram.writeLock.load(mode === 3)
    dma.readLock.load(mode >= 2 || on && xTicked === Gb.lineCycles - 1 &&
      (lyTicked < Gb.visibleLines - 1 || lyTicked === Gb.frameLines - 1))
    dma.writeLock.load(mode === 3 || mode === 2 && !lastOfScan)
    // The OAM row the scan reads, for the OAM corruption bug; see `PpuReference.oamBugRow`.
    dma.scanRow.load((mode === 2 && xTicked < Gb.oamEnd - 1) ? (xTicked + 1).resize(5) | U(0, 5 bits))

    regs.read(coincidenceTicked, Gb.STAT, 2)
    regs.read(mode, Gb.STAT, 0)
    regs.read(scy, Gb.SCY)
    regs.read(scx, Gb.SCX)
    regs.read(lyRead, Gb.LY)
    regs.readAndWrite(lyc, Gb.LYC)
    regs.read(bgp, Gb.BGP)
    regs.read(obp0, Gb.OBP0)
    regs.read(obp1, Gb.OBP1)
    regs.readAndWrite(wy, Gb.WY)
    regs.read(wx, Gb.WX)

    for ((r, a) <- Seq(lcdc -> Gb.LCDC, scy -> Gb.SCY, scx -> Gb.SCX, bgp -> Gb.BGP, obp0 -> Gb.OBP0, obp1 -> Gb.OBP1, wx -> Gb.WX))
      when(writes(a)) { r := wData }
    x := xTicked
    ly := lyTicked
    statLine := line
    coincidence := coincidenceTicked
    lycLine := lycLineTicked
    // An LYC write compares at once while LY is compared.
    when(slave.writeFire && slave.cmd.address === Gb.LYC && on && compared) { lycLine := lyNow === slave.cmd.data }
    firstLine := firstLineTicked
    irqModeLast := irqMode
    wyHit := wyHitTicked
  }
}
