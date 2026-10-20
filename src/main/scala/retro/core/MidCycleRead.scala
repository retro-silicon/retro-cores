package retro.core

import spinal.core._

/** A memory read that FPGA block RAM can do, with the data there by the end of the CPU cycle,
  * as from `readAsync`. The address must settle early in the cycle (it comes from registers,
  * never from data read in the same cycle), and a write in the cycle lands at its end, so the
  * read sees the old value, as `readAsync` does.
  *
  * When the core's fast clock is faster than the CPU's (`CoreClocks.divided`), the read is
  * registered on every fast clock edge; otherwise on the falling edge of the CPU clock. */
object MidCycleRead {
  def apply[T <: Data](mem: Mem[T], address: UInt): T = {
    val clocks = RetroCore.clocks.get
    val domain = if (clocks.divided) clocks.fast else ClockDomain.current.withRevertedClockEdge()
    new ClockingArea(domain) {
      val data = mem.readSync(address, clockCrossing = true)
    }.data
  }
}
