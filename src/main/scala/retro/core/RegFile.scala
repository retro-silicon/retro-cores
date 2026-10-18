package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin._

import scala.collection.mutable.ArrayBuffer

case class RegSpec(name: String, width: Int, init: BigInt = 0)
/** A register pair such as HL: reads as hi ## lo, a write updates both halves. */
case class PairSpec(name: String, hi: String, lo: String)
/** A flag is one bit of a register (SM83 Z = F[7]); it reads as a Bool and can be written alone. */
case class FlagSpec(name: String, reg: String, bit: Int)

case class RegFileSpec(regs: Seq[RegSpec], pairs: Seq[PairSpec] = Nil, flags: Seq[FlagSpec] = Nil) {
  val names = regs.map(_.name) ++ pairs.map(_.name) ++ flags.map(_.name)
  require(names.distinct.size == names.size, s"duplicate register/pair/flag names in $names")
  def reg(name: String) = regs.find(_.name == name).getOrElse(sys.error(s"no register $name"))
  def widthOf(name: String): Int = pairs.find(_.name == name) match {
    case Some(p) => reg(p.hi).width + reg(p.lo).width
    case None if flags.exists(_.name == name) => 1
    case None => reg(name).width
  }
}

trait RegFileService {
  def spec: RegFileSpec
  /** Retain this before calling `newWrite` from a build phase. */
  def regFileLock: Retainer
  /** Current value of a register, pair or flag (flags read as 1-bit UInt). */
  def read(name: String): UInt
  def flag(name: String): Bool
  /** A write port onto a register, pair or flag. Ports registered later win within a cycle. */
  def newWrite(name: String): Flow[UInt]
}

/** An 8-bit-style register file described as data: plain registers, aliased pairs and flags.
  *
  * When a `SequencerService` is present every register, pair and flag is published to it as a
  * source and a sink (so opcode specs can say `"HL" := "rdata"`), and every flag as a branch
  * condition, both `"Z"` and `"!Z"`. */
class RegFilePlugin(val spec: RegFileSpec) extends FiberPlugin with RegFileService {
  val regFileLock = Retainer()
  private val writes = ArrayBuffer[(String, Flow[UInt])]()

  val regs = during setup new Area {
    val byName = spec.regs.map(r => r.name -> Reg(UInt(r.width bits)).init(r.init).setName(s"reg_${r.name}").simPublic()).toMap
  }

  def read(name: String): UInt = spec.pairs.find(_.name == name) match {
    case Some(p) => regs.byName(p.hi) @@ regs.byName(p.lo)
    case None if spec.flags.exists(_.name == name) => flag(name).asUInt
    case None => regs.byName(spec.reg(name).name)
  }
  def flag(name: String): Bool = {
    val f = spec.flags.find(_.name == name).getOrElse(sys.error(s"no flag $name"))
    regs.byName(f.reg)(f.bit)
  }
  def newWrite(name: String): Flow[UInt] = {
    val port = Flow(UInt(spec.widthOf(name) bits)).setCompositeName(this, s"write_$name")
    writes += name -> port
    port
  }

  val logic = during setup new Area {
    val seq = host.get[SequencerService]
    val seqRetainer = seq.map(s => retains(s.seqLock))
    awaitBuild()

    for (s <- seq) {
      for (name <- spec.names) {
        s.addSource(name, read(name))
        s.addSink(name, newWrite(name))
      }
      for (f <- spec.flags) {
        s.addCond(f.name, flag(f.name))
        s.addCond("!" + f.name, !flag(f.name))
      }
    }
    seqRetainer.foreach(_.release())

    regFileLock.await()
    // Whole registers and pairs first, then flags, so a flag write overrides a register write in the same cycle.
    val (flagWrites, regWrites) = writes.partition { case (n, _) => spec.flags.exists(_.name == n) }
    for ((name, port) <- regWrites ++ flagWrites) when(port.valid) {
      spec.pairs.find(_.name == name) match {
        case Some(p) =>
          val loWidth = spec.reg(p.lo).width
          regs.byName(p.hi) := port.payload(port.payload.getWidth - 1 downto loWidth)
          regs.byName(p.lo) := port.payload(loWidth - 1 downto 0)
        case None => spec.flags.find(_.name == name) match {
          case Some(f) => regs.byName(f.reg)(f.bit) := port.payload(0)
          case None => regs.byName(name) := port.payload
        }
      }
    }
  }
}
