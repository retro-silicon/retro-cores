package retro.core

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._

import scala.collection.mutable

/** Introspection without touching the plugins under observation: for every stage it taps
  * `down.isValid` and the given payloads into named, `simPublic` signals a sim probe can read.
  * Taps are made before the pipeline is built, so the Builder propagates the payloads to them. */
class WhiteboxPlugin(payloads: Payload[_ <: Data]*) extends FiberPlugin {
  val logic = during setup new Area {
    val pp = host[StagePipelinePlugin]
    val retainer = retains(pp.tapLock)
    awaitBuild()

    val valid = mutable.LinkedHashMap[Int, Bool]()
    val values = mutable.LinkedHashMap[(Int, Payload[_ <: Data]), Data]()
    for (id <- pp.stages.get) {
      val node = pp.ctrl(id).down
      valid(id) = CombInit(node.isValid).setName(s"whitebox_s${id}_valid").simPublic()
      for (p <- payloads) values(id -> p) = CombInit(node(p)).setName(s"whitebox_s${id}_${p.getName()}").simPublic()
    }
    retainer.release()
  }
}
