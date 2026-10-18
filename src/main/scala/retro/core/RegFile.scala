package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin._

import scala.collection.mutable.ArrayBuffer

/** A register, pair or flag: something opcode specs can both read and write. */
sealed trait Storage extends Source with Sink { def width: Int }

class Register(val name: String, val width: Int, val init: BigInt = 0) extends Storage
/** A register pair such as HL: reads as hi ## lo, a write updates both halves. */
class Pair(val name: String, val hi: Register, val lo: Register) extends Storage { def width = hi.width + lo.width }
/** A flag is one bit of a register (SM83 Z = F[7]); it reads as a Bool and can be written alone.
  * `isSet` and `isClear` are the branch conditions the register file publishes for it. */
class Flag(val name: String, val reg: Register, val bit: Int) extends Storage {
  def width = 1
  val isSet = Cond(name)
  val isClear = Cond(s"!$name")
}

object Register { def apply(name: String, width: Int, init: BigInt = 0) = new Register(name, width, init) }
object Pair { def apply(name: String, hi: Register, lo: Register) = new Pair(name, hi, lo) }
object Flag { def apply(name: String, reg: Register, bit: Int) = new Flag(name, reg, bit) }

case class RegFileSpec(regs: Seq[Register], pairs: Seq[Pair] = Nil, flags: Seq[Flag] = Nil) {
  val all: Seq[Storage] = regs ++ pairs ++ flags
  require(all.map(_.name).distinct.size == all.size, s"duplicate register/pair/flag names in ${all.mkString(", ")}")
  for (p <- pairs) require(regs.contains(p.hi) && regs.contains(p.lo), s"pair $p uses a register not in the spec")
  for (f <- flags) require(regs.contains(f.reg), s"flag $f uses a register not in the spec")
}

trait RegFileService {
  def spec: RegFileSpec
  /** Retain this before calling `newWrite` from a build phase. */
  def regFileLock: Retainer
  /** Current value of a register, pair or flag (flags read as 1-bit UInt). */
  def read(s: Storage): UInt
  def flag(f: Flag): Bool
  /** A write port onto a register, pair or flag. Ports registered later win within a cycle. */
  def newWrite(s: Storage): Flow[UInt]
}

/** An 8-bit-style register file described as data: plain registers, aliased pairs and flags.
  *
  * When a `SequencerService` is present every register, pair and flag is published to it as a
  * source and a sink, and every flag's `isSet` / `isClear` as branch conditions. */
class RegFilePlugin(val spec: RegFileSpec) extends FiberPlugin with RegFileService {
  val regFileLock = Retainer()
  private val writes = ArrayBuffer[(Storage, Flow[UInt])]()

  val regs = during setup new Area {
    val byReg = spec.regs.map(r => r -> Reg(UInt(r.width bits)).init(r.init).setName(s"reg_${r.name}").simPublic()).toMap
  }

  def read(s: Storage): UInt = s match {
    case r: Register => regs.byReg(r)
    case p: Pair => regs.byReg(p.hi) @@ regs.byReg(p.lo)
    case f: Flag => flag(f).asUInt
  }
  def flag(f: Flag): Bool = regs.byReg(f.reg)(f.bit)
  def newWrite(s: Storage): Flow[UInt] = {
    val port = Flow(UInt(s.width bits)).setCompositeName(this, s"write_${s.name}")
    writes += s -> port
    port
  }

  val logic = during setup new Area {
    val seq = host.get[SequencerService]
    val seqRetainer = seq.map(s => retains(s.seqLock))
    awaitBuild()

    for (s <- seq) {
      for (st <- spec.all) {
        s.addSource(st, read(st))
        s.addSink(st, newWrite(st))
      }
      for (f <- spec.flags) {
        s.addCond(f.isSet, flag(f))
        s.addCond(f.isClear, !flag(f))
      }
    }
    seqRetainer.foreach(_.release())

    regFileLock.await()
    // Whole registers and pairs first, then flags, so a flag write overrides a register write in the same cycle.
    val (flagWrites, regWrites) = writes.partition(_._1.isInstanceOf[Flag])
    for ((target, port) <- regWrites ++ flagWrites) when(port.valid) {
      target match {
        case r: Register => regs.byReg(r) := port.payload
        case p: Pair =>
          regs.byReg(p.hi) := port.payload(p.width - 1 downto p.lo.width)
          regs.byReg(p.lo) := port.payload(p.lo.width - 1 downto 0)
        case f: Flag => regs.byReg(f.reg)(f.bit) := port.payload(0)
      }
    }
  }
}
