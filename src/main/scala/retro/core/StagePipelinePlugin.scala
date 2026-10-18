package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._

import scala.collection.mutable

/** A chain of `CtrlLink`s created lazily by id, so any plugin can place logic "at stage n"
  * without the owner knowing the stage count in advance (VexiiRiscv's ExecutePipelinePlugin trick).
  *
  * Elaboration order:
  *  1. contributors `retains(elaborationLock)` in setup, then call `ctrl(n)` in build and release;
  *  2. the stage list is frozen and published through `stages`;
  *  3. introspection plugins holding `tapLock` read nodes on the known stages, then release;
  *  4. the StageLinks are added and `pipeline.Builder` connects and propagates payloads. */
class StagePipelinePlugin extends FiberPlugin {
  val elaborationLock = Retainer()
  val tapLock = Retainer()
  val stages = Handle[Seq[Int]]()

  private val idToCtrl = mutable.LinkedHashMap[Int, CtrlLink]()
  def ctrl(id: Int): CtrlLink = idToCtrl.getOrElseUpdate(id, CtrlLink().setCompositeName(this, s"ctrl$id"))

  val logic = during build new Area {
    elaborationLock.await()
    val ids = 0 to idToCtrl.keys.max
    ids.foreach(ctrl)
    stages.load(ids)

    tapLock.await()
    val stageLinks = for (i <- ids.dropRight(1)) yield StageLink(ctrl(i).down, ctrl(i + 1).up)
    Builder(ids.map(ctrl) ++ stageLinks)
  }
}
