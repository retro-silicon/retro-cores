package retro.core

import spinal.core._

/** A memory read that FPGA block RAM can do: registered on the falling clock edge, so the data
  * is there by the end of the cycle, as from `readAsync`. The address must settle in the first
  * half of the cycle (it comes from registers, never from data read in the same cycle) and a
  * write in the cycle lands at the rising edge after it, so the read sees the old value, as
  * `readAsync` does. */
object MidCycleRead {
  def apply[T <: Data](mem: Mem[T], address: UInt): T = {
    val falling = ClockDomain.current.withRevertedClockEdge()
    new ClockingArea(falling) {
      val data = mem.readSync(address, clockCrossing = true)
    }.data
  }
}
