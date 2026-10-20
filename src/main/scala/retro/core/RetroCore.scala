package retro.core

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin._

/** The clocks a `RetroCore` runs on. `fast` is the component's own clock; the plugins run on
  * `cpu`, which is `fast` enabled in the last of every `divider` cycles. `phase` counts the fast
  * cycles of a CPU cycle from 0, so `cpu` ticks at the end of phase `divider - 1`. With a divider
  * of 1 the two are the same clock and `phase` is always 0. A plugin that works per fast cycle
  * (a video chip's dots) builds that logic in a `ClockingArea(fast)`. */
case class CoreClocks(fast: ClockDomain, cpu: ClockDomain, phase: UInt, divider: Int)

/** Empty-host toplevel, the VexiiRiscv pattern: the core is nothing but its plugin list.
  * Every fiber the plugins start runs with `database` in scope, so `Database.blocking` keys
  * are visible to all of them, and in the `cpu` clock domain (see `CoreClocks`). */
class RetroCore(val clockDivider: Int = 1) extends Component {
  val database = new Database
  val fast = ClockDomain.current
  val phase = Reg(UInt(log2Up(clockDivider max 2) bits)).init(0).simPublic()
  val cpuEnable = Bool()
  if (clockDivider > 1) {
    phase := (phase === clockDivider - 1) ? U(0, phase.getWidth bits) | (phase + 1)
    cpuEnable := phase === clockDivider - 1
  } else {
    phase := 0
    cpuEnable := True
  }
  val cpu = if (clockDivider > 1) fast.copy(clockEnable = cpuEnable) else fast
  database(RetroCore.clocks) = CoreClocks(fast, cpu, phase, clockDivider)
  // The host captures the clock domain its plugins' fibers run in.
  val host = database on (cpu on (new PluginHost))
}

object RetroCore {
  val clocks = Database.value[CoreClocks]()
  def apply(plugins: scala.collection.Seq[Hostable], clockDivider: Int = 1): RetroCore = {
    val core = new RetroCore(clockDivider)
    core.host.asHostOf(plugins)
    core
  }
}
