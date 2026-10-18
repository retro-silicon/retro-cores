package retro.sim

import retro.core._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

case class BusAccess(write: Boolean, address: Int, data: Int) {
  override def toString = if (write) f"W $address%04X <- $data%02X" else f"R $address%04X -> $data%02X"
}

/** One clock. `done` is false when the bus held the cycle; such cycles repeat the same step. */
case class CycleEvent(cycle: Long, opcode: Int, step: Int, done: Boolean, bus: Option[BusAccess], regs: Map[String, BigInt])

/** One instruction, from the M-cycle after its opcode fetch to the fetch of the next opcode.
  * `address` is where its opcode was fetched; `regs` are the values after it completed. */
case class RetireEvent(index: Long, address: Int, opcode: Int, startCycle: Long, mcycles: Int, stalls: Int,
                       bus: Seq[BusAccess], regs: Map[String, BigInt])

trait TraceBackend {
  def onCycle(e: CycleEvent): Unit = {}
  def onRetire(e: RetireEvent): Unit = {}
}

class RecordingBackend extends TraceBackend {
  val cycles = ArrayBuffer[CycleEvent]()
  val retired = ArrayBuffer[RetireEvent]()
  override def onCycle(e: CycleEvent) = cycles += e
  override def onRetire(e: RetireEvent) = retired += e
}

class PrintBackend(names: Map[Int, String] = Map.empty, out: String => Unit = println) extends TraceBackend {
  override def onRetire(e: RetireEvent) = {
    val regs = e.regs.toSeq.sortBy(_._1).map { case (n, v) => s"$n=${v.toString(16).toUpperCase}" }.mkString(" ")
    out(f"${e.startCycle}%6d  ${e.address}%04X  ${e.opcode}%02X  ${names.getOrElse(e.opcode, "?")}%-12s ${e.mcycles}M${if (e.stalls > 0) s"+${e.stalls}" else ""}  $regs")
  }
}

/** Sim side of the introspection chain (whitebox plugin, then this probe, then backends): samples
  * the `TracePlugin` taps every clock and turns them into cycle and retire events. */
class Probe(trace: TracePlugin, cd: ClockDomain) {
  val backends = ArrayBuffer[TraceBackend]()
  def add(b: TraceBackend): this.type = { backends += b; this }

  private val t = trace.logic.get
  private var cycle = 0L
  private var retired = 0L
  // Instruction currently executing, known once its opcode fetch has been seen.
  private var current: Option[(Int, Int, Long)] = None // (address, opcode, start cycle)
  private var mcycles, stalls = 0
  private val bus = ArrayBuffer[BusAccess]()
  private var pendingRetire: Option[RetireEvent] = None

  private def regs() = t.regs.map { case (n, r) => n -> r.toBigInt }.toMap

  // Values read right after a sampling edge are the ones that edge sampled.
  cd.onSamplings {
    val access = if (t.busValid.toBoolean) Some(BusAccess(t.busWrite.toBoolean, t.busAddress.toInt,
      if (t.busWrite.toBoolean) t.busWdata.toInt else t.busRdata.toInt)) else None
    val done = t.done.toBoolean
    val now = regs()
    val e = CycleEvent(cycle, t.opcode.toInt, t.step.toInt, done, access, now)
    // Register writes of the previous fetch cycle are visible now.
    for (r <- pendingRetire) { backends.foreach(_.onRetire(r.copy(regs = now))); pendingRetire = None }
    backends.foreach(_.onCycle(e))

    if (done) {
      mcycles += 1
      access.foreach(bus += _)
      if (t.fetch.toBoolean) {
        for ((addr, op, start) <- current) {
          pendingRetire = Some(RetireEvent(retired, addr, op, start, mcycles, stalls, bus.toList, Map.empty))
          retired += 1
        }
        current = Some((access.get.address, access.get.data, cycle + 1))
        mcycles = 0; stalls = 0; bus.clear()
      }
    } else stalls += 1
    cycle += 1
  }
}
