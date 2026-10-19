package retro.gb

import org.scalatest.funsuite.AnyFunSuite

/** Known answers for the reference model's peripherals, from the Pan Docs and the Mooneye and
  * Blargg test ROMs, so the system cosim is anchored to hardware behaviour and not only to
  * RTL/model agreement. Each bus call is one M-cycle; its tick happens before its access. */
class GbReferenceSpec extends AnyFunSuite {
  def fresh() = new GbReference(Array.fill(0x10000)(0), postBoot = false)

  test("DIV counts every 64 M-cycles and resets on write") {
    val gb = fresh()
    gb.write(Gb.DIV, 0x55)
    for (_ <- 0 until 62) gb.idle()
    assert(gb.read(Gb.DIV) == 0) // 63 cycles after the reset
    assert(gb.read(Gb.DIV) == 1) // 64, as Mooneye div_timing expects
  }

  /** TIMA at FE, TMA 40, fastest rate. The tick before the DIV write takes counter bit 3 low and
    * TIMA to FF; after the reset the fourth tick is the next falling edge of bit 3 and
    * overflows it. */
  def armed() = {
    val gb = fresh()
    gb.write(Gb.TAC, 0x05)
    gb.write(Gb.TMA, 0x40)
    gb.write(Gb.TIMA, 0xFE)
    gb.write(Gb.DIV, 0) // the tick takes the counter from 12 to 16: bit 3 falls
    assert(gb.read(Gb.TIMA) == 0xFF)
    gb.idle()
    assert(gb.read(Gb.TIMA) == 0xFF) // the next tick overflows it
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

  test("serial: an internally clocked byte shifts on the 8192 Hz clock and shifts in FF") {
    val gb = fresh() // counter 0: bit 8 falls every 128 M-cycles, first in M-cycle 128
    gb.write(Gb.SB, 0x42)
    gb.write(Gb.SC, 0x81) // M-cycle 2
    for (_ <- 0 until 1020) gb.idle()
    assert(gb.read(Gb.SC) == 0xFF) // M-cycle 1023: busy
    assert(gb.read(Gb.SC) == 0x7F) // 1024: the eighth falling edge
    assert(gb.read(Gb.SB) == 0xFF)
    assert(gb.read(Gb.IF) == 0xE8)
    assert(gb.serialOut == Seq(0x42))
  }

  test("LCD: LY advances every 114 M-cycles and VBlank is flagged entering line 144") {
    val gb = fresh()
    gb.write(Gb.LCDC, 0x80) // on from the next tick, which starts line 0
    for (_ <- 0 until 111) gb.idle()
    assert(gb.read(Gb.LY) == 0)
    assert(gb.read(Gb.LY) == 1) // the line's last M-cycle: LY already reads the next line
    for (_ <- 0 until 142 * 114 - 1) gb.idle()
    assert(gb.read(Gb.LY) == 143 && gb.read(Gb.IF) == 0xE0)
    for (_ <- 0 until 112) gb.idle()
    assert(gb.read(Gb.LY) == 144)
    assert(gb.read(Gb.IF) == 0xE1)
    assert((gb.read(Gb.STAT) & 3) == 1)
  }
}
