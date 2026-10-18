package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.plugin._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import SequencerPlugin.Field

object SequencerPlugin {
  /** A slice of the decoded control word. */
  case class Field(name: String, width: Int, offset: Int)
}

trait SequencerService {
  /** Retain this before adding ops, sources, sinks, conds or keys from a build phase. */
  def seqLock: Retainer
  def addOp(op: OpSpec): Unit
  def addSource(name: String, value: UInt): Unit
  def addSink(name: String, port: Flow[UInt]): Unit
  def addCond(name: String, value: Bool): Unit
  def newKey(name: String, width: Int, default: BigInt = 0): CtrlKey
}

/** The M-cycle sequencer: a plain FSM holding the opcode register (IR) and the M-cycle counter
  * (STEP). Each clock is one M-cycle. A decoder generated from every collected `OpSpec` maps
  * IR ## STEP to a control word: the bus access, a source selection for each sink, a branch,
  * and plugin-defined keys. When the bus stalls the cycle is held and nothing is written.
  *
  * @param illegal  what an opcode with no spec does, one M-cycle (default: fetch at PC, PC+1)
  * @param resetOpcode  IR at reset; its first M-cycle runs first, so make it one that fetches */
class SequencerPlugin(val opcodeWidth: Int = 8,
                      val busPriority: Int = 0,
                      val resetOpcode: Int = 0,
                      val illegal: MCycle = MCycle.fetch("PC").assign("PC" -> "PC+1")) extends FiberPlugin with SequencerService {
  val seqLock = Retainer()
  private val ops = mutable.LinkedHashMap[Int, OpSpec]()
  private val sources = mutable.LinkedHashMap[String, UInt]()
  private val sinks = mutable.LinkedHashMap[String, Flow[UInt]]()
  private val conds = mutable.LinkedHashMap[String, Bool]()
  private val keys = ArrayBuffer[CtrlKey]()

  def addOp(op: OpSpec) = {
    require(!ops.contains(op.opcode), f"opcode 0x${op.opcode}%02X defined twice (${ops(op.opcode).name}, ${op.name})")
    ops(op.opcode) = op
  }
  def addSource(name: String, value: UInt) = { require(!sources.contains(name), s"source $name defined twice"); sources(name) = value }
  def addSink(name: String, port: Flow[UInt]) = { require(!sinks.contains(name), s"sink $name defined twice"); sinks(name) = port }
  def addCond(name: String, value: Bool) = { require(!conds.contains(name), s"cond $name defined twice"); conds(name) = value }
  def newKey(name: String, width: Int, default: BigInt = 0): CtrlKey = {
    val k = new CtrlKey(name, width, default)
    k.value = UInt(width bits).setCompositeName(this, s"key_$name")
    keys += k
    k
  }
  def opSpecs: Seq[OpSpec] = ops.values.toSeq

  val logic = during setup new Area {
    val bus = host[BusService]
    val busRetainer = retains(bus.busLock)
    awaitBuild()

    val master = bus.newMaster(busPriority)
    busRetainer.release()
    addSource("rdata", master.rdata)

    seqLock.await()

    // ---- Control word layout -------------------------------------------------------------
    val sourceNames = sources.keys.toIndexedSeq
    val condNames = conds.keys.toIndexedSeq
    val maxCycles = (ops.values.map(_.cycles.size) ++ Seq(1)).max
    val stepWidth = log2Up(maxCycles) max 1
    val selWidth = log2Up(sourceNames.size + 1)
    val condWidth = log2Up(condNames.size + 1)

    val fields = ArrayBuffer[Field]()
    def field(name: String, width: Int) = { val f = Field(name, width, fields.map(_.width).sum); fields += f; f }
    val fKind = field("busKind", 2) // 0 idle, 1 read, 2 write, 3 fetch
    val fAddr = field("addr", selWidth)
    val fWdata = field("wdata", selWidth)
    val fSinks = sinks.keys.map(n => n -> field(s"sink_$n", selWidth)).toMap
    val fCond = field("cond", condWidth)
    val fTarget = field("target", stepWidth)
    val fKeys = keys.map(k => k -> field(s"key_${k.name}", k.width)).toMap
    val wordWidth = fields.map(_.width).sum

    def src(op: String, name: String): BigInt = {
      val i = sourceNames.indexOf(name)
      require(i >= 0, s"$op: unknown source '$name' (have ${sourceNames.mkString(", ")})")
      i + 1
    }
    def encode(op: String, c: MCycle): BigInt = {
      var w = BigInt(0)
      def put(f: Field, v: BigInt) = { require(v >= 0 && v < (BigInt(1) << f.width), s"$op: ${f.name}=$v does not fit"); w |= v << f.offset }
      c.bus match {
        case BusOp.Idle =>
        case BusOp.Read(a) => put(fKind, 1); put(fAddr, src(op, a))
        case BusOp.Write(a, d) => put(fKind, 2); put(fAddr, src(op, a)); put(fWdata, src(op, d))
        case BusOp.Fetch(a) => put(fKind, 3); put(fAddr, src(op, a))
      }
      require(c.assigns.map(_._1).distinct.size == c.assigns.size, s"$op: a sink is assigned twice in one cycle")
      for ((sink, source) <- c.assigns) put(fSinks.getOrElse(sink, sys.error(s"$op: unknown sink '$sink'")), src(op, source))
      for (b <- c.branch) {
        val i = condNames.indexOf(b.cond)
        require(i >= 0, s"$op: unknown cond '${b.cond}' (have ${condNames.mkString(", ")})")
        put(fCond, i + 1); put(fTarget, b.target)
      }
      val set = c.keys.toMap
      for (k <- keys) put(fKeys(k), set.getOrElse(k, k.default))
      w
    }

    // ---- Generated decoder ---------------------------------------------------------------
    val ir = Reg(Bits(opcodeWidth bits)) init resetOpcode
    val step = Reg(UInt(stepWidth bits)) init 0

    val spec = new DecodingSpec(HardType(Bits(wordWidth bits)))
    val all = BigInt(1) << (opcodeWidth + stepWidth)
    val fullCare = all - 1
    val opcodeCare = fullCare ^ ((BigInt(1) << stepWidth) - 1)
    spec.setDefault(Masked(encode("<illegal>", illegal), (BigInt(1) << wordWidth) - 1))
    for (op <- ops.values; (c, i) <- op.cycles.zipWithIndex)
      spec.addNeeds(Masked((BigInt(op.opcode) << stepWidth) | i, fullCare), Masked(encode(op.name, c), (BigInt(1) << wordWidth) - 1))
    // Undefined opcodes take the illegal word at any step; unused steps of defined opcodes are don't-care.
    val undefined = (0 until (1 << opcodeWidth)).filterNot(ops.contains).map(o => Masked(BigInt(o) << stepWidth, opcodeCare))
    val word = spec.build(ir ## step, undefined)
    def slice(f: Field) = word(f.offset, f.width bits).asUInt

    // ---- Datapath --------------------------------------------------------------------------
    def select(sel: UInt, width: Int): UInt = {
      val r = UInt(width bits)
      r := 0
      switch(sel) {
        for ((name, i) <- sourceNames.zipWithIndex) is(i + 1) { r := sources(name).resized }
        if (sourceNames.size + 1 < (1 << selWidth)) default {}
      }
      r
    }

    val kind = slice(fKind)
    val fetch = kind === 3
    master.cmd.valid := kind =/= 0
    master.cmd.write := kind === 2
    master.cmd.address := select(slice(fAddr), bus.busConfig.addressWidth)
    master.cmd.data := select(slice(fWdata), bus.busConfig.dataWidth)
    val done = !master.stall

    for ((name, port) <- sinks) {
      val sel = slice(fSinks(name))
      port.valid := done && sel =/= 0
      port.payload := select(sel, port.payload.getWidth)
    }
    for (k <- keys) k.value := slice(fKeys(k))

    val condSel = slice(fCond)
    // Index 0 means "no branch", so prepend a constant False below the published conds.
    val condTaken = (conds.values.toSeq.asBits() ## False)(condSel)

    when(done) {
      when(fetch) {
        ir := master.rdata.asBits.resized
        step := 0
      } elsewhen (condTaken) {
        step := slice(fTarget)
      } otherwise {
        step := step + 1
      }
    }
  }
}
