package retro.gb

import org.scalatest.funsuite.AnyFunSuite

/** Known answers for the reference model's timer, from the Pan Docs description, so the system
  * cosim is anchored to documented behaviour and not only to RTL/model agreement. Each bus call
  * is one M-cycle. */
class GbReferenceSpec extends AnyFunSuite {
  def fresh() = new GbReference(Array.fill(0x10000)(0), postBoot = false)

  test("DIV counts every 64 M-cycles and resets on write") {
    val gb = fresh()
    gb.write(Gb.DIV, 0x55)
    for (_ <- 0 until 63) gb.idle()
    assert(gb.read(Gb.DIV) == 0) // 63 cycles after the reset
    assert(gb.read(Gb.DIV) == 1) // 64
  }

  /** TIMA at FE, TMA 40, fastest rate. Writing DIV with counter bit 3 high ticks TIMA (FF), the
    * next falling edge of bit 3 comes 4 cycles later and overflows it. */
  def armed() = {
    val gb = fresh()
    gb.write(Gb.TAC, 0x05)
    gb.write(Gb.TMA, 0x40)
    gb.write(Gb.TIMA, 0xFE)
    gb.write(Gb.DIV, 0) // counter was 12: bit 3 falls
    assert(gb.read(Gb.TIMA) == 0xFF)
    gb.idle(); gb.idle()
    assert(gb.read(Gb.TIMA) == 0xFF) // overflows at the end of this cycle
    gb
  }

  test("TIMA overflow: reads 00 for one cycle, then TMA, then the interrupt flag") {
    val gb = armed()
    assert(gb.read(Gb.TIMA) == 0x00)
    assert(gb.read(Gb.TIMA) == 0x40)
    assert(gb.read(Gb.IF) == 0xE4)
  }

  test("writing TIMA in the overflow cycle cancels the reload and the interrupt") {
    val gb = armed()
    gb.write(Gb.TIMA, 0x12)
    assert(gb.read(Gb.TIMA) == 0x12)
    assert(gb.read(Gb.IF) == 0xE0)
  }

  test("in the reload cycle a TIMA write is ignored and a TMA write reaches TIMA") {
    val gb = armed()
    gb.idle()
    gb.write(Gb.TIMA, 0x12)
    assert(gb.read(Gb.TIMA) == 0x40)
    val gb2 = armed()
    gb2.idle()
    gb2.write(Gb.TMA, 0x99)
    assert(gb2.read(Gb.TIMA) == 0x99)
  }

  test("serial: an internally clocked byte takes 1024 M-cycles and shifts in FF") {
    val gb = fresh()
    gb.write(Gb.SB, 0x42)
    gb.write(Gb.SC, 0x81)
    for (_ <- 0 until 1023) gb.idle()
    assert(gb.read(Gb.SC) == 0xFF) // 1024th cycle after the start: busy until its end
    assert(gb.read(Gb.SC) == 0x7F)
    assert(gb.read(Gb.SB) == 0xFF)
    assert(gb.read(Gb.IF) == 0xE8)
    assert(gb.serialOut == Seq(0x42))
  }

  test("LCD: LY advances every 114 M-cycles and VBlank is flagged entering line 144") {
    val gb = fresh()
    gb.write(Gb.LCDC, 0x80) // on from the next cycle, at line 0 dot 0
    for (_ <- 0 until 113) gb.idle()
    assert(gb.read(Gb.LY) == 0)
    assert(gb.read(Gb.LY) == 1)
    for (_ <- 0 until 142 * 114 - 1) gb.idle()
    assert(gb.read(Gb.LY) == 143 && gb.read(Gb.IF) == 0xE0)
    for (_ <- 0 until 112) gb.idle()
    assert(gb.read(Gb.LY) == 144)
    assert(gb.read(Gb.IF) == 0xE1)
    assert((gb.read(Gb.STAT) & 3) == 1)
  }
}
