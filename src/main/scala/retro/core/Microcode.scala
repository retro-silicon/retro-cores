package retro.core

/** Opcodes described as data. An opcode is a list of M-cycles; each M-cycle names at most one bus
  * access and a set of register transfers. Everything an opcode can name is a typed object
  * declared once by whoever provides its hardware, the way VexiiRiscv declares its `SrcKeys` and
  * `Payload`s: registers, pairs and flags in a `RegFileSpec`, computed values (`Value`), branch
  * conditions (`Cond`) and control keys (`CtrlKey`) in a plugin's companion object. A typo is a
  * compile error; a value no plugin provides is an elaboration error naming the opcode.
  *
  * {{{
  * // SM83 LD A,(HL+): read at HL into A while the IDU bumps HL, then fetch the next opcode
  * OpSpec("LD A,(HL+)", 0x2A, Seq(
  *   MCycleSpec.read(HL).assign(A -> SequencerPlugin.RDATA, HL -> Idu.inc(HL)),
  *   MCycleSpec.fetch(PC).assign(PC -> Idu.inc(PC))))
  * }}}
  */

/** Something an M-cycle can read: a bus address, write data, or the right side of a transfer. */
trait Source { def name: String; override def toString = name }
/** Something an M-cycle can write at the end of the cycle. */
trait Sink { def name: String; override def toString = name }

/** A value computed by a plugin from the current state (an adder, the ALU result, a constant).
  * The plugin publishes its hardware with `SequencerService.addSource(value, hardware)`. */
class Value(val name: String) extends Source
object Value { def apply(name: String) = new Value(name) }

/** A branch condition, published with `SequencerService.addCond`. */
class Cond(val name: String) { override def toString = name }
object Cond { def apply(name: String) = new Cond(name) }

/** A control signal decoded per (opcode, M-cycle). The owning plugin registers it with
  * `SequencerService.addKey` and reads the decoded value with `SequencerService.ctrl`. */
class CtrlKey(val name: String, val width: Int, val default: BigInt = 0) { override def toString = name }
object CtrlKey { def apply(name: String, width: Int, default: BigInt = 0) = new CtrlKey(name, width, default) }

sealed trait BusOp
object BusOp {
  case object Idle extends BusOp
  case class Read(address: Source) extends BusOp
  case class Write(address: Source, data: Source) extends BusOp
  /** Read the next opcode: ends the instruction, the read data becomes the new opcode. */
  case class Fetch(address: Source) extends BusOp
  /** Read the second byte of a prefixed opcode (SM83 0xCB): IR becomes page ## data and the new
    * opcode starts at M-cycle 0, but this is not an instruction boundary. */
  case class Prefix(address: Source, page: Int) extends BusOp
}

/** At the end of this cycle, jump to M-cycle `target` of the same opcode when `cond` holds. */
case class Branch(cond: Cond, target: Int)

/** What one M-cycle of an opcode does. */
case class MCycleSpec(bus: BusOp = BusOp.Idle,
                      assigns: Seq[(Sink, Source)] = Nil,
                      keys: Seq[(CtrlKey, BigInt)] = Nil,
                      branch: Option[Branch] = None) {
  /** Register transfers, as `sink -> source`, applied at the end of the cycle. */
  def assign(pairs: (Sink, Source)*): MCycleSpec = copy(assigns = assigns ++ pairs)
  /** Set a plugin-defined control key for this cycle. */
  def set(key: CtrlKey, value: BigInt): MCycleSpec = copy(keys = keys :+ (key -> value))
  def branchIf(cond: Cond, target: Int): MCycleSpec = copy(branch = Some(Branch(cond, target)))
  def isFetch = bus.isInstanceOf[BusOp.Fetch]
  def loadsOpcode = isFetch || bus.isInstanceOf[BusOp.Prefix]
}

object MCycleSpec {
  def idle = MCycleSpec()
  def read(address: Source) = MCycleSpec(BusOp.Read(address))
  def write(address: Source, data: Source) = MCycleSpec(BusOp.Write(address, data))
  def fetch(address: Source) = MCycleSpec(BusOp.Fetch(address))
  def prefix(address: Source, page: Int) = MCycleSpec(BusOp.Prefix(address, page))
}

case class OpSpec(name: String, opcode: Int, cycles: Seq[MCycleSpec]) {
  // Earlier cycles may fetch too, when a branch reaches them (SM83 HALT has two exits).
  require(cycles.nonEmpty && cycles.last.loadsOpcode, s"$name: the last M-cycle must be a fetch or a prefix")
  for (c <- cycles; b <- c.branch) require(b.target >= 0 && b.target < cycles.size, s"$name: branch target ${b.target} out of range")
}
