package retro.core

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin._

/** The clocks a `RetroCore` runs on. `fast` is the component's own clock; the plugins run on
  * `cpu`, which is `fast` enabled in the last fast cycle of each CPU cycle. A CPU cycle lasts
  * `cycleClocks` fast cycles, the pattern repeating (`Seq(3)`: every third; `Seq(3, 3, 3, 3, 4)`:
  * 3.2 on average). `phase` counts the fast cycles of a CPU cycle from 0, and `cpuEnable` is high
  * in the last. With `Seq(1)` the two clocks are the same and `phase` is always 0. A plugin that
  * works per fast cycle (a video chip's dots) builds that logic in a `ClockingArea(fast)`. */
case class CoreClocks(fast: ClockDomain, cpu: ClockDomain, phase: UInt, cpuEnable: Bool, cycleClocks: Seq[Int]) {
  /** Whether the fast clock is faster than the CPU's. */
  def divided = cycleClocks.exists(_ > 1)
}

/** Empty-host toplevel, the VexiiRiscv pattern: the core is nothing but its plugin list.
  * Every fiber the plugins start runs with `database` in scope, so `Database.blocking` keys
  * are visible to all of them, and in the `cpu` clock domain (see `CoreClocks`). */
class RetroCore(val cycleClocks: Seq[Int] = Seq(1)) extends Component {
  require(cycleClocks.nonEmpty && cycleClocks.forall(_ >= 1))
  val database = new Database
  val fast = ClockDomain.current
  val phase = Reg(UInt(log2Up(cycleClocks.max max 2) bits)).init(0).simPublic()
  val cpuEnable = Bool().simPublic()
  if (cycleClocks.exists(_ > 1)) {
    val last =
      if (cycleClocks.distinct.size == 1) U(cycleClocks.head - 1, phase.getWidth bits)
      else {
        // Which CPU cycle of the pattern this is.
        val index = Reg(UInt(log2Up(cycleClocks.size) bits)).init(0)
        when(cpuEnable) { index := (index === cycleClocks.size - 1) ? U(0, index.getWidth bits) | (index + 1) }
        Vec(cycleClocks.map(c => U(c - 1, phase.getWidth bits)))(index)
      }
    cpuEnable := phase === last
    phase := cpuEnable ? U(0, phase.getWidth bits) | (phase + 1)
  } else {
    phase := 0
    cpuEnable := True
  }
  val cpu = if (cycleClocks.exists(_ > 1)) fast.copy(clockEnable = cpuEnable) else fast
  database(RetroCore.clocks) = CoreClocks(fast, cpu, phase, cpuEnable, cycleClocks)
  // The host captures the clock domain its plugins' fibers run in.
  val host = database on (cpu on (new PluginHost))
}

object RetroCore {
  val clocks = Database.value[CoreClocks]()
  def apply(plugins: scala.collection.Seq[Hostable], cycleClocks: Seq[Int]): RetroCore = {
    val core = new RetroCore(cycleClocks)
    core.host.asHostOf(plugins)
    core
  }
  /** A core whose CPU cycle is `clockDivider` fast cycles. */
  def apply(plugins: scala.collection.Seq[Hostable], clockDivider: Int = 1): RetroCore = apply(plugins, Seq(clockDivider))
}
