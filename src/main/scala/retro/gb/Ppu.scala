package retro.gb

import retro.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._

/** Up to four pixels a cycle (one per dot) for the LCD: `valid(i)` marks the pixels sent, `x(i)`
  * their columns on line `y`, `shade(i)` their shades, 0 (white) to 3 (black). */
case class LcdPixels() extends Bundle {
  val valid = Bits(4 bits)
  val x = Vec(UInt(8 bits), 4)
  val y = UInt(8 bits)
  val shade = Vec(UInt(2 bits), 4)
}

/** The pixel transfer state that changes dot by dot; see `PpuReference`, which this mirrors. */
case class PpuDot() extends Bundle {
  val transfer = Bool()
  val discard = UInt(3 bits)
  val pos8 = UInt(8 bits)
  // Background fetcher
  val fStep = UInt(3 bits)
  val fTileX = UInt(5 bits)
  val fTile, fLo, fHi = UInt(8 bits)
  val fWindow, winActive, winUsed = Bool()
  // Background FIFO and object FIFO (bit 7 leaves first)
  val bgLo, bgHi = Bits(8 bits)
  val bgCount = UInt(4 bits)
  val oLo, oHi, oPal, oPrio = Bits(8 bits)
  // Object fetch
  val sActive = Bool()
  val sStep = UInt(3 bits)
  val sSlot = UInt(4 bits)
  val sLo = Bits(8 bits)
  val fetched = Bits(10 bits)
}

/** The DMG PPU: LCDC, STAT, SCY, SCX, LY, LYC, BGP, OBP0, OBP1, WY, WX, the VBlank and STAT
  * interrupts, and the pixels, sent out on `lcd`. It reads VRAM and OAM through their own ports
  * (`RamPlugin.directRead`, `OamDmaPlugin.oamRead`), not over the CPU's bus.
  *
  * The design runs one clock per M-cycle and the PPU works in dots, four per M-cycle, so each
  * clock chains four copies of the one-dot step (`dot`), each with its own VRAM read port; the
  * OAM scan checks two OAM entries a clock. The timing, and what it leaves out, is described at
  * `PpuReference`, the model the system cosim checks this against dot for dot. */
class PpuPlugin(resetLcdc: Int = 0) extends GbPeripheral(SizeMapping(Gb.LCDC, 0x0C), Gb.Irq.VBLANK, Gb.Irq.STAT) {
  var lcd: LcdPixels = null
  def body(slave: BusSlave, irq: Seq[Bool]) = new Area {
    lcd = out(LcdPixels()).setName("lcd")
    val vram = host.list[RamPlugin].find(_.ramName == "vram").get
    val dma = host[OamDmaPlugin]

    val lcdc = Reg(UInt(8 bits)).init(resetLcdc).simPublic()
    val statSelect = Reg(UInt(4 bits)) init 0
    val scy, scx, lyc, bgp, obp0, obp1, wy, wx = Reg(UInt(8 bits)) init 0
    val x = Reg(UInt(7 bits)).init(0).simPublic()
    val ly = Reg(UInt(8 bits)).init(0).simPublic()
    val statLine = RegInit(False)
    val coincidence = RegInit(False)
    val firstLine = RegInit(False)
    val on = lcdc(7)
    val tall = lcdc(2)

    val objY, objX, objTile, objAttr = Vec.fill(10)(Reg(UInt(8 bits)) init 0)
    val objCount = Reg(UInt(4 bits)) init 0
    val wyHit = RegInit(False)
    val winLine = Reg(UInt(8 bits)) init 0
    val st = Reg(PpuDot())
    st.transfer init False
    st.winUsed init False
    st.fetched init 0

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

    // Line start, then the window's line trigger and the OAM scan.
    val s0 = PpuDot()
    s0 := st
    val winLineTicked = UInt(8 bits)
    val wyHitTicked = Bool()
    val countScanned = UInt(4 bits)
    winLineTicked := winLine
    wyHitTicked := wyHit
    countScanned := objCount
    when(lineStart) {
      when(lyTicked === 0) {
        wyHitTicked := False
        winLineTicked := 0
      } elsewhen (st.winUsed) {
        winLineTicked := winLine + 1
      }
      s0.winUsed := False
      s0.fetched := 0
      s0.transfer := False
      countScanned := 0
    }
    when(visible && lyTicked === wy) { wyHitTicked := True }

    val height = tall ? U(16, 8 bits) | U(8, 8 bits)
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
      objTile(slot.resized) := scan(j).byte(2)
      objAttr(slot.resized) := scan(j).byte(3)
    }
    objCount := slot1 + take1.asUInt

    when(visible && xTicked === Gb.oamEnd) {
      s0.transfer := True
      s0.pos8 := 0
      s0.discard := scx(2 downto 0)
      s0.fStep := 2
      s0.fTileX := 0x1F
      s0.fTile := 0
      s0.fWindow := False
      s0.winActive := False
      s0.bgCount := 0
      s0.oLo := 0; s0.oHi := 0; s0.oPal := 0; s0.oPrio := 0
      s0.sActive := False
    }

    // ---- The pixel transfer: four dots ---------------------------------------------------
    def dot(s: PpuDot, i: Int) = new Area {
      val n = PpuDot()
      n := s
      val pixel = Flow(UInt(2 bits))
      pixel.valid := False
      pixel.payload := 0
      val vAddr = UInt(13 bits)
      vAddr := 0
      val vData = vram.directRead(vAddr).asBits

      val bgRow = s.fWindow ? winLineTicked | (lyTicked + scy)
      def tileData(high: Int) = {
        val upper = !lcdc(4) && !s.fTile(7)
        (upper.asUInt @@ s.fTile @@ bgRow(2 downto 0) @@ U(high, 1 bits))
      }
      val mapBase = (s.fWindow ? lcdc(6) | lcdc(3)) ? U(0x1C00, 13 bits) | U(0x1800, 13 bits)
      val col = s.fWindow ? s.fTileX | (scx(7 downto 3) + s.fTileX)
      val mapAddr = mapBase + (bgRow(7 downto 3) @@ col).resized

      // The object being fetched: its row (flipped) and the address of a row byte.
      val sy = objY(s.sSlot.resized)
      val sAttr = objAttr(s.sSlot.resized)
      val r = (lyTicked + 16 - sy)(3 downto 0)
      val row = sAttr(6) ? (r ^ (tall ? U(15, 4 bits) | U(7, 4 bits))) | r
      val sTile = objTile(s.sSlot.resized) & (tall ? U(0xFE, 8 bits) | U(0xFF, 8 bits))
      def objAddr(high: Int) = ((sTile @@ U(0, 4 bits)) + (row @@ U(high, 1 bits))).resize(13)
      val objByte = sAttr(5) ? vData.reversed | vData

      val winTrigger = !s.winActive && lcdc(5) && wyHitTicked && s.discard === 0 && s.pos8.resize(9) === wx +^ 1

      // Background fetcher; a push also counts as the next fetch's first step.
      val push = s.fStep === 6 && s.bgCount === 0
      val fStepAfter = push ? U(1, 3 bits) | ((s.fStep === 6) ? U(6, 3 bits) | s.fStep + 1)
      val bgLoAfter = push ? s.fLo.asBits | s.bgLo
      val bgHiAfter = push ? s.fHi.asBits | s.bgHi
      val bgCountAfter = push ? U(8, 4 bits) | s.bgCount

      val hits = (0 until 10).map(k => k < objCount && !s.fetched(k) && objX(k) === s.pos8)
      val hit = lcdc(1) && s.discard === 0 && hits.orR
      val hitSlot = OHToUInt(OHMasking.first(hits.asBits()))

      when(!s.transfer) {
      } elsewhen (s.sActive) {
        n.sStep := s.sStep + 1
        when(s.sStep + 1 === 4) {
          vAddr := objAddr(0)
          n.sLo := objByte
        }
        when(s.sStep + 1 === 6) {
          vAddr := objAddr(1)
          for (b <- 0 until 8) when(!(s.oLo(b) || s.oHi(b))) {
            n.oLo(b) := s.sLo(b)
            n.oHi(b) := objByte(b)
            n.oPal(b) := sAttr(4)
            n.oPrio(b) := sAttr(7)
          }
          n.fetched(s.sSlot) := True
          n.sActive := False
        }
      } elsewhen (winTrigger) {
        n.winActive := True
        n.winUsed := True
        n.fWindow := True
        n.fTileX := 0
        n.fStep := 1
        n.bgCount := 0
      } otherwise {
        n.fStep := fStepAfter
        when(push) {
          n.bgLo := bgLoAfter
          n.bgHi := bgHiAfter
          n.bgCount := 8
          n.fTileX := s.fTileX + 1
        }
        switch(s.fStep) {
          is(1) { vAddr := mapAddr; n.fTile := vData.asUInt }
          is(3) { vAddr := tileData(0).resized; n.fLo := vData.asUInt }
          is(5) { vAddr := tileData(1).resized; n.fHi := vData.asUInt }
          default {}
        }
        when(hit) {
          when(fStepAfter === 6 && bgCountAfter =/= 0) {
            n.sActive := True
            n.sStep := 1
            n.sSlot := hitSlot.resized
          }
        } elsewhen (bgCountAfter =/= 0) {
          val bg = bgHiAfter(7) ## bgLoAfter(7)
          n.bgLo := bgLoAfter |<< 1
          n.bgHi := bgHiAfter |<< 1
          n.bgCount := bgCountAfter - 1
          when(s.discard =/= 0) {
            n.discard := s.discard - 1
          } otherwise {
            val obj = s.oHi(7) ## s.oLo(7)
            n.oLo := s.oLo |<< 1
            n.oHi := s.oHi |<< 1
            n.oPal := s.oPal |<< 1
            n.oPrio := s.oPrio |<< 1
            val c = lcdc(0) ? bg | B"00"
            val objShown = lcdc(1) && obj =/= 0 && !(s.oPrio(7) && c =/= 0)
            val palette = objShown ? (s.oPal(7) ? obp1 | obp0) | bgp
            val index = objShown ? obj | c
            pixel.valid := s.pos8 >= 8
            pixel.payload := palette.subdivideIn(2 bits)(index.asUInt)
            n.pos8 := s.pos8 + 1
            when(s.pos8 === 167) { n.transfer := False }
          }
        }
      }
    }.setCompositeName(this, s"dot$i")

    val dots = scala.collection.mutable.ArrayBuffer[Area { val n: PpuDot; val pixel: Flow[UInt] }]()
    for (i <- 0 until 4) dots += dot(if (i == 0) s0 else dots(i - 1).n, i)
    st := dots.last.n
    for (i <- 0 until 4) {
      lcd.valid(i) := dots(i).pixel.valid
      lcd.x(i) := (if (i == 0) s0.pos8 else dots(i - 1).n.pos8) - 8
      lcd.shade(i) := dots(i).pixel.payload
    }
    lcd.y := lyTicked

    // STAT reads the mode after the third dot; the STAT interrupt sees the transfer as it was
    // before the dots. The first line after the LCD is turned on has no mode 2.
    val scanMode = firstLineTicked ? U(0, 2 bits) | U(2, 2 bits)
    val mode, irqMode = UInt(2 bits)
    mode := 0
    irqMode := 0
    when(on) {
      when(lyTicked >= Gb.visibleLines) {
        mode := 1
        irqMode := 1
      } elsewhen (xTicked < Gb.oamEnd) {
        mode := scanMode
        irqMode := scanMode
      } otherwise {
        when(dots(2).n.transfer) { mode := 3 }
        when(xTicked === Gb.oamEnd) { irqMode := scanMode } elsewhen (s0.transfer) { irqMode := 3 }
      }
    }
    // LY = LYC is only compared while the LCD is on, and stays on the interrupt line when off.
    val coincidenceTicked = on ? (lyTicked === lyc) | coincidence
    val line = (statSelect(3) && coincidenceTicked) || on && ((statSelect(2) && irqMode === 2) ||
      (statSelect(1) && irqMode === 1) || (statSelect(0) && irqMode === 0))
    // LY reads the next line in the last M-cycle of a line.
    val lyRead = (on && xTicked === Gb.lineCycles - 1) ? ((lyTicked === Gb.frameLines - 1) ? U(0, 8 bits) | lyTicked + 1) | lyTicked
    irq(0) := vblank
    irq(1) := line && !statLine

    val regs = new RetroBusSlaveFactory(slave, idleBits = 0xFF)
    regs.readAndWrite(lcdc, Gb.LCDC)
    regs.readAndWrite(statSelect, Gb.STAT, 3)
    regs.read(coincidenceTicked, Gb.STAT, 2)
    regs.read(mode, Gb.STAT, 0)
    regs.readAndWrite(scy, Gb.SCY)
    regs.readAndWrite(scx, Gb.SCX)
    regs.read(lyRead, Gb.LY)
    regs.readAndWrite(lyc, Gb.LYC)
    regs.readAndWrite(bgp, Gb.BGP)
    regs.readAndWrite(obp0, Gb.OBP0)
    regs.readAndWrite(obp1, Gb.OBP1)
    regs.readAndWrite(wy, Gb.WY)
    regs.readAndWrite(wx, Gb.WX)

    x := xTicked
    ly := lyTicked
    statLine := line
    coincidence := coincidenceTicked
    firstLine := firstLineTicked
    winLine := winLineTicked
    wyHit := wyHitTicked
  }
}
