package retro.demo

import retro.core._
import spinal.core._
import spinal.lib._
import spinal.lib.misc.database.Database
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._

/** A toy core that exercises every framework mechanism the retro CPUs will use:
  * a parameter in the database, plugins injecting logic into stages they don't own,
  * a halt on a control link, and whitebox taps. It is not a CPU. */
object Demo {
  val WIDTH = Database.blocking[Int]()
  val VALUE = Payload(UInt(WIDTH.get bits)).setName("VALUE")
}
import Demo._

/** Stage 0: emits an incrementing counter; stalls when downstream halts. Owns the WIDTH parameter. */
class SourcePlugin(width: Int) extends FiberPlugin {
  during setup { WIDTH.set(width) }

  val logic = during setup new Area {
    val pp = host[StagePipelinePlugin]
    val retainer = retains(pp.elaborationLock)
    awaitBuild()

    val c = pp.ctrl(0)
    val counter = Reg(UInt(WIDTH.get bits)) init 0
    c.up.valid := True
    c.up(VALUE) := counter
    when(c.up.isFiring) { counter := counter + 1 }
    retainer.release()
  }
}

/** Adds a constant to VALUE as it passes stage `at`. Several of these compose without knowing each other. */
class AddPlugin(at: Int, amount: Int) extends FiberPlugin {
  val logic = during setup new Area {
    val pp = host[StagePipelinePlugin]
    val retainer = retains(pp.elaborationLock)
    awaitBuild()

    val c = pp.ctrl(at)
    c.bypass(VALUE) := c.up(VALUE) + amount
    retainer.release()
  }
}

/** Holds stage `at` while the `stall` input is high, standing in for a bus wait state. */
class HaltPlugin(at: Int) extends FiberPlugin {
  val logic = during setup new Area {
    val pp = host[StagePipelinePlugin]
    val retainer = retains(pp.elaborationLock)
    awaitBuild()

    val stall = in Bool()
    pp.ctrl(at).haltWhen(stall)
    retainer.release()
  }
}

/** Drives the result out of stage `at` as a Flow. */
class SinkPlugin(at: Int) extends FiberPlugin {
  val logic = during setup new Area {
    val pp = host[StagePipelinePlugin]
    val retainer = retains(pp.elaborationLock)
    awaitBuild()

    val c = pp.ctrl(at)
    val result = master(Flow(UInt(WIDTH.get bits)))
    result.valid := c.down.isFiring
    result.payload := c.down(VALUE)
    retainer.release()
  }
}

object DemoCore {
  val amounts = Seq(1 -> 3, 2 -> 10)

  def plugins = Seq(
    new StagePipelinePlugin,
    new SourcePlugin(width = 8),
    new HaltPlugin(at = 1),
    new SinkPlugin(at = 3),
    new WhiteboxPlugin(VALUE)
  ) ++ amounts.map { case (at, k) => new AddPlugin(at, k) }

  def apply(): RetroCore = RetroCore(plugins).setDefinitionName("DemoCore")
}

object DemoVerilog extends App {
  SpinalConfig(targetDirectory = "gen").generateVerilog(DemoCore())
}
