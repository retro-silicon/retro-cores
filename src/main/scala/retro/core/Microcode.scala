package retro.core

/** Opcodes described as data. An opcode is a list of M-cycles; each M-cycle names at most one bus
  * access and a set of register transfers, by the names of sources and sinks other plugins have
  * published to the sequencer. The sequencer turns the collected specs into a decoder.
  *
  * {{{
  * // SM83 LD A,(HL): read at HL into A, then fetch the next opcode
  * OpSpec("LD A,(HL)", 0x7E, Seq(
  *   MCycle.read("HL").assign("A" -> "rdata"),
  *   MCycle.fetch("PC").assign("PC" -> "PC+1")))
  * }}}
  */
sealed trait BusOp
object BusOp {
  case object Idle extends BusOp
  case class Read(address: String) extends BusOp
  case class Write(address: String, data: String) extends BusOp
  /** Read the next opcode: ends the instruction, the read data becomes the new opcode. */
  case class Fetch(address: String) extends BusOp
}

/** At the end of this cycle, jump to M-cycle `target` of the same opcode when `cond` holds. */
case class Branch(cond: String, target: Int)

case class MCycle(bus: BusOp = BusOp.Idle,
                  assigns: Seq[(String, String)] = Nil,
                  keys: Seq[(CtrlKey, BigInt)] = Nil,
                  branch: Option[Branch] = None) {
  /** Register transfers, as `sink -> source`, applied at the end of the cycle. */
  def assign(pairs: (String, String)*): MCycle = copy(assigns = assigns ++ pairs)
  /** Set a plugin-defined control key for this cycle. */
  def set(key: CtrlKey, value: BigInt): MCycle = copy(keys = keys :+ (key -> value))
  def branchIf(cond: String, target: Int): MCycle = copy(branch = Some(Branch(cond, target)))
  def isFetch = bus.isInstanceOf[BusOp.Fetch]
}

object MCycle {
  def idle = MCycle()
  def read(address: String) = MCycle(BusOp.Read(address))
  def write(address: String, data: String) = MCycle(BusOp.Write(address, data))
  def fetch(address: String) = MCycle(BusOp.Fetch(address))
}

case class OpSpec(name: String, opcode: Int, cycles: Seq[MCycle]) {
  require(cycles.nonEmpty && cycles.last.isFetch, s"$name: the last M-cycle must be a fetch")
  require(cycles.init.forall(!_.isFetch), s"$name: only the last M-cycle may fetch")
  for (c <- cycles; b <- c.branch) require(b.target > 0 && b.target < cycles.size, s"$name: branch target ${b.target} out of range")
}

/** A control signal defined by a plugin and decoded per (opcode, M-cycle) by the sequencer.
  * `value` is valid hardware once the plugin's build phase runs; read it to gate your logic. */
class CtrlKey(val name: String, val width: Int, val default: BigInt) {
  var value: spinal.core.UInt = null
}
