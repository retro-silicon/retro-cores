package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.misc.plugin._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import SequencerPlugin.Field

object SequencerPlugin {
  /** The bus read data of the current M-cycle. */
  val RDATA = Value("rdata")
  /** A slice of the decoded control word. */
  case class Field(name: String, width: Int, offset: Int)
}

trait SequencerService {
  /** Retain this before adding ops, sources, sinks, conds or keys from a build phase. */
  def seqLock: Retainer
  def addOp(op: OpSpec): Unit
  /** Provide the hardware behind a source. Every source an opcode names needs exactly one.
    * `fromRdata` marks a value computed from `rdata`: it is kept out of the bus address and
    * write data multiplexers, which would otherwise close a loop through on-chip slaves. */
  def addSource(source: Source, value: UInt, fromRdata: Boolean = false): Unit
  def addSink(sink: Sink, port: Flow[UInt]): Unit
  def addCond(cond: Cond, value: Bool): Unit
  /** Declare a control key; the owning plugin calls this once, in build, before releasing. */
  def addKey(key: CtrlKey): Unit
  /** The decoded value of a key in the current cycle. Valid hardware once the key is added. */
  def ctrl(key: CtrlKey): UInt
  /** When `valid` is high on a completing fetch, IR loads `opcode` instead of the fetched byte
    * (which is discarded). This is how interrupt dispatch enters its own opcode. */
  def addFetchOverride(valid: Bool, opcode: UInt): Unit
  /** High in the cycle where an instruction-ending fetch completes. Usable from any build phase. */
  def fetchFire: Bool
  /** High when the current M-cycle completes (the bus did not hold it). Usable from any build phase. */
  def cycleDone: Bool
  /** The bus read data of the current M-cycle, as hardware, for values computed from it (the 6502
    * pulls P from the stack as `rdata` with two bits forced). Usable from any build phase. A source
    * made from it is added with `fromRdata = true`. */
  def rdata: UInt
  /** IR: the opcode of the current M-cycle, page and overrides included. Usable from a build
    * phase that holds no `seqLock`. */
  def opcode: Bits
  /** The CPU's own bus request, before arbitration (a DMA that halts the CPU on a read watches
    * it). Usable from a build phase that holds no `seqLock`. */
  def master: BusMaster
}

/** The M-cycle sequencer: a plain FSM holding the opcode register (IR) and the M-cycle counter
  * (STEP). Each clock is one M-cycle. A decoder generated from every collected `OpSpec` maps
  * IR ## STEP to a control word: the bus access, a source selection for each sink, a branch,
  * and plugin-defined keys. When the bus stalls the cycle is held and nothing is written.
  *
  * @param illegal  what an opcode with no spec does, in one M-cycle (usually: fetch at PC, PC+1)
  * @param resetOpcode  IR at reset; its first M-cycle runs first, so make it one that fetches
  * @param romDecoder  decode with a ROM indexed by IR ## STEP instead of minimized logic: the same
  *                    words (unused steps of defined opcodes give the illegal word rather than
  *                    don't-care), and several times cheaper to evaluate in Verilator */
class SequencerPlugin(val opcodeWidth: Int = 8, val busPriority: Int = 0, val resetOpcode: Int = 0, val illegal: MCycleSpec, val romDecoder: Boolean = false)
    extends FiberPlugin
    with SequencerService {
  val seqLock = Retainer()
  private val ops = mutable.LinkedHashMap[Int, OpSpec]()
  private val sources = mutable.LinkedHashMap[Source, UInt]()
  private val sinks = mutable.LinkedHashMap[Sink, Flow[UInt]]()
  private val conds = mutable.LinkedHashMap[Cond, Bool]()
  private val keys = mutable.LinkedHashMap[CtrlKey, UInt]()
  private val overrides = ArrayBuffer[(Bool, UInt)]()

  val early = during setup new Area {
    val fetchFire, cycleDone = Bool()
    val rdata = UInt(host[BusService].busConfig.dataWidth bits)
  }
  def fetchFire = early.fetchFire
  def cycleDone = early.cycleDone
  def rdata = early.rdata
  def opcode = logic.ir
  def master = logic.master
  def addFetchOverride(valid: Bool, opcode: UInt) = overrides += valid -> opcode

  def addOp(op: OpSpec) = {
    require(!ops.contains(op.opcode), f"opcode 0x${op.opcode}%02X defined twice (${ops(op.opcode).name}, ${op.name})")
    ops(op.opcode) = op
  }
  private val rdataSources = mutable.Set[Source](SequencerPlugin.RDATA)
  def addSource(source: Source, value: UInt, fromRdata: Boolean = false) = {
    require(!sources.contains(source), s"source $source provided twice")
    sources(source) = value
    if (fromRdata) rdataSources += source
  }
  def addSink(sink: Sink, port: Flow[UInt]) = { require(!sinks.contains(sink), s"sink $sink provided twice"); sinks(sink) = port }
  def addCond(cond: Cond, value: Bool) = { require(!conds.contains(cond), s"cond $cond provided twice"); conds(cond) = value }
  def addKey(key: CtrlKey) = {
    require(!keys.contains(key), s"key $key added twice")
    keys(key) = UInt(key.width bits).setCompositeName(this, s"key_${key.name}")
  }
  def ctrl(key: CtrlKey): UInt = keys.getOrElse(key, sys.error(s"key $key was never added; its plugin must call addKey"))
  def opSpecs: Seq[OpSpec] = ops.values.toSeq

  val logic = during setup new Area {
    val bus = host[BusService]
    val busRetainer = retains(bus.busLock)
    awaitBuild()

    val master = bus.newMaster(busPriority)
    busRetainer.release()
    early.rdata := master.rdata
    addSource(SequencerPlugin.RDATA, master.rdata)

    seqLock.await()

    // ---- Control word layout -------------------------------------------------------------
    val sourceList = sources.keys.toIndexedSeq
    val condList = conds.keys.toIndexedSeq
    val maxCycles = (ops.values.map(_.cycles.size) ++ Seq(1)).max
    val stepWidth = log2Up(maxCycles) max 1
    val selWidth = log2Up(sourceList.size + 1)
    val condWidth = log2Up(condList.size + 1)

    val fields = ArrayBuffer[Field]()
    def field(name: String, width: Int) = { val f = Field(name, width, fields.map(_.width).sum); fields += f; f }
    val fKind = field("busKind", 2) // 0 idle, 1 read, 2 write, 3 fetch or prefix
    val dataWidth = bus.busConfig.dataWidth
    val pageWidth = (opcodeWidth - dataWidth) max 1
    val fPrefix = field("prefix", 1)
    val fPage = field("page", pageWidth)
    val fAddr = field("addr", selWidth)
    val fWdata = field("wdata", selWidth)
    val fSinks = sinks.keys.map(n => n -> field(s"sink_${n.name}", selWidth)).toMap
    val fCond = field("cond", condWidth)
    val fTarget = field("target", stepWidth)
    val fKeys = keys.keys.map(k => k -> field(s"key_${k.name}", k.width)).toMap
    val wordWidth = fields.map(_.width).sum

    def src(op: String, source: Source): BigInt = {
      val i = sourceList.indexOf(source)
      require(i >= 0, s"$op: no plugin provides source '$source'")
      i + 1
    }
    // The bus address and write data cannot come from the bus read data of the same cycle: with
    // on-chip slaves that would be a combinational loop through the address decoder.
    def addr(op: String, source: Source): BigInt = {
      require(!rdataSources(source), s"$op: the bus address or write data cannot be RDATA or a value computed from it")
      src(op, source)
    }
    def encode(op: String, c: MCycleSpec): BigInt = {
      var w = BigInt(0)
      def put(f: Field, v: BigInt) = { require(v >= 0 && v < (BigInt(1) << f.width), s"$op: ${f.name}=$v does not fit"); w |= v << f.offset }
      c.bus match {
        case BusOp.Idle =>
        case BusOp.Read(a) => put(fKind, 1); put(fAddr, addr(op, a))
        case BusOp.Write(a, d) => put(fKind, 2); put(fAddr, addr(op, a)); put(fWdata, addr(op, d))
        case BusOp.Fetch(a) => put(fKind, 3); put(fAddr, addr(op, a))
        case BusOp.Prefix(a, page) => put(fKind, 3); put(fAddr, addr(op, a)); put(fPrefix, 1); put(fPage, page)
      }
      require(c.assigns.map(_._1).distinct.size == c.assigns.size, s"$op: a sink is assigned twice in one cycle")
      for ((sink, source) <- c.assigns) put(fSinks.getOrElse(sink, sys.error(s"$op: no plugin provides sink '$sink'")), src(op, source))
      for (b <- c.branch) {
        val i = condList.indexOf(b.cond)
        require(i >= 0, s"$op: no plugin provides cond '${b.cond}'")
        put(fCond, i + 1); put(fTarget, b.target)
      }
      val set = c.keys.toMap
      for (k <- set.keys) require(keys.contains(k), s"$op: key $k was never added")
      for (k <- keys.keys) put(fKeys(k), set.getOrElse(k, k.default))
      w
    }

    // ---- Generated decoder ---------------------------------------------------------------
    val ir = Reg(Bits(opcodeWidth bits)).init(resetOpcode).simPublic()
    val step = Reg(UInt(stepWidth bits)).init(0)

    val spec = new DecodingSpec(HardType(Bits(wordWidth bits)))
    val all = BigInt(1) << (opcodeWidth + stepWidth)
    val fullCare = all - 1
    val opcodeCare = fullCare ^ ((BigInt(1) << stepWidth) - 1)
    spec.setDefault(Masked(encode("<illegal>", illegal), (BigInt(1) << wordWidth) - 1))
    for (op <- ops.values; (c, i) <- op.cycles.zipWithIndex)
      spec.addNeeds(Masked((BigInt(op.opcode) << stepWidth) | i, fullCare), Masked(encode(op.name, c), (BigInt(1) << wordWidth) - 1))
    // Undefined opcodes take the illegal word at any step; unused steps of defined opcodes are don't-care.
    val undefined = (0 until (1 << opcodeWidth)).filterNot(ops.contains).map(o => Masked(BigInt(o) << stepWidth, opcodeCare))
    val word = if (romDecoder) {
      val table = Array.fill(1 << (opcodeWidth + stepWidth))(encode("<illegal>", illegal))
      for (op <- ops.values; (c, i) <- op.cycles.zipWithIndex) table((op.opcode << stepWidth) | i) = encode(op.name, c)
      Mem(Bits(wordWidth bits), table.map(w => B(w, wordWidth bits)).toSeq).readAsync((ir ## step).asUInt)
    } else spec.build(ir ## step, undefined)
    def slice(f: Field) = word(f.offset, f.width bits).asUInt

    // ---- Datapath --------------------------------------------------------------------------
    // The sources some microcode puts on the bus. Only those feed its address and data, so
    // values that depend on the bus access (interrupt requests a write can raise) do not loop.
    val busSources = ops.values
      .flatMap(_.cycles.map(_.bus))
      .flatMap {
        case BusOp.Idle => Nil
        case BusOp.Read(a) => Seq(a)
        case BusOp.Write(a, d) => Seq(a, d)
        case BusOp.Fetch(a) => Seq(a)
        case BusOp.Prefix(a, _) => Seq(a)
      }
      .toSet
    def select(sel: UInt, width: Int, busSide: Boolean = false): UInt = {
      val r = UInt(width bits)
      r := 0
      switch(sel) {
        for ((source, i) <- sourceList.zipWithIndex if !busSide || busSources(source) && !rdataSources(source)) is(i + 1) { r := sources(source).resized }
        if (sourceList.size + 1 < (1 << selWidth)) default {}
      }
      r
    }

    val kind = slice(fKind)
    val fetch = kind === 3 && !slice(fPrefix).asBool
    val prefix = kind === 3 && slice(fPrefix).asBool
    master.cmd.valid := kind =/= 0
    master.cmd.write := kind === 2
    master.cmd.address := select(slice(fAddr), bus.busConfig.addressWidth, busSide = true)
    master.cmd.data := select(slice(fWdata), bus.busConfig.dataWidth, busSide = true)
    val done = !master.stall

    for ((name, port) <- sinks) {
      val sel = slice(fSinks(name))
      port.valid := done && sel =/= 0
      port.payload := select(sel, port.payload.getWidth)
    }
    for ((k, value) <- keys) value := slice(fKeys(k))

    val condSel = slice(fCond)
    // Index 0 means "no branch", so prepend a constant False below the published conds.
    val condTaken = (conds.values.toSeq.asBits() ## False)(condSel)

    fetchFire := done && fetch
    cycleDone := done
    // Later-registered overrides win.
    val fetched = master.rdata.asBits.resize(opcodeWidth)
    val nextOpcode = overrides.foldLeft(fetched) { case (acc, (valid, op)) => valid ? op.asBits.resize(opcodeWidth) | acc }

    when(done) {
      when(fetch) {
        ir := nextOpcode
        step := 0
      } elsewhen (prefix) {
        ir := (slice(fPage).asBits ## master.rdata.asBits).resize(opcodeWidth)
        step := 0
      } elsewhen (condTaken) {
        step := slice(fTarget)
      } otherwise {
        step := step + 1
      }
    }
  }
}
