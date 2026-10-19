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
  * `address` is where its opcode was fetched. `opcode` is the full IR value: prefixed opcodes
  * include their page (SM83 CB xx = 0x1xx), and an overridden fetch reports the opcode that
  * replaced it (interrupt dispatch). `regs` are the values after it completed. */
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
  * the `TracePlugin` taps every clock and turns them into cycle and retire events. With
  * `cycleEvents = false` no `onCycle` is sent and registers are read only when an instruction
  * retires, which makes long system runs much faster. With `autoSample = false` the caller
  * drives the clock and calls `sample()` once per cycle, just before the rising edge. */
class Probe(trace: TracePlugin, cd: ClockDomain, cycleEvents: Boolean = true, autoSample: Boolean = true) {
  val backends = ArrayBuffer[TraceBackend]()
  def add(b: TraceBackend): this.type = { backends += b; this }

  private val t = trace.logic.get
  private var cycle = 0L
  private var retired = 0L
  // Instruction currently executing, known once its opcode fetch has been seen.
  private var current: Option[(Int, Int, Long)] = None // (address, opcode, start cycle)
  // IR loaded at the last edge; read it in the next cycle rather than trusting the fetched byte.
  private var opcodeLoaded = false
  private var mcycles, stalls = 0
  private val bus = ArrayBuffer[BusAccess]()
  private var pendingRetire: Option[RetireEvent] = None

  private def regs(): Map[String, BigInt] = t.regWords.flatMap { case (word, fields) =>
    val w = word.toLong
    fields.map { case (n, o, width) => n -> BigInt((w >>> o) & ((1L << width) - 1)) }
  }.toMap
  private val dw = t.busWdata.getBitsWidth
  private val aw = t.busAddress.getBitsWidth
  require(2 * dw + aw + 5 <= 63)
  private def field(c: Long, offset: Int, width: Int) = ((c >> offset) & ((1L << width) - 1)).toInt

  // Values read right after a sampling edge are the ones that edge sampled.
  if (autoSample) cd.onSamplings(sample())
  def sample(): Unit = {
    val c = t.cycle.toLong
    val flags = 2 * dw + aw
    def flag(i: Int) = ((c >> (flags + i)) & 1) != 0 // prefix, fetch, done, write, valid
    val write = flag(3)
    val access = if (flag(4)) Some(BusAccess(write, field(c, 2 * dw, aw), field(c, if (write) dw else 0, dw))) else None
    val done = flag(2)
    lazy val now = regs()
    if (opcodeLoaded) { current = current.map { case (a, _, s) => (a, t.opcode.toInt, s) }; opcodeLoaded = false }
    // Register writes of the previous fetch cycle are visible now.
    for (r <- pendingRetire) { backends.foreach(_.onRetire(r.copy(regs = now))); pendingRetire = None }
    if (cycleEvents) {
      val e = CycleEvent(cycle, t.opcode.toInt, t.step.toInt, done, access, now)
      backends.foreach(_.onCycle(e))
    }

    if (done) {
      mcycles += 1
      access.foreach(bus += _)
      if (flag(1)) {
        for ((addr, op, start) <- current) {
          pendingRetire = Some(RetireEvent(retired, addr, op, start, mcycles, stalls, bus.toList, Map.empty))
          retired += 1
        }
        current = Some((access.get.address, -1, cycle + 1))
        opcodeLoaded = true
        mcycles = 0; stalls = 0; bus.clear()
      } else if (flag(0)) opcodeLoaded = true
    } else stalls += 1
    cycle += 1
  }
}
