package retro.toy

import retro.core._
import retro.core.MCycleSpec.{idle, read, write}
import retro.core.SequencerPlugin.RDATA
import spinal.core._
import spinal.lib.misc.plugin._

/** A toy 8-bit CPU with SM83-like encodings and timings, used only to test the framework. */
object Toy8 {
  val A = Register("A", 8); val F = Register("F", 8); val H = Register("H", 8); val L = Register("L", 8)
  val W = Register("W", 8); val Z = Register("Z", 8); val PC = Register("PC", 16)
  val HL = Pair("HL", H, L); val WZ = Pair("WZ", W, Z)
  val ZF = Flag("ZF", F, 7)
  val regFile = RegFileSpec(Seq(A, F, H, L, W, Z, PC), Seq(HL, WZ), Seq(ZF))

  // Values the ISA plugin computes.
  val PC_INC = Value("PC+1"); val HL_INC = Value("HL+1"); val A_INC = Value("A+1")
  val A_INC_ZERO = Value("zero(A+1)"); val PC_E = Value("PC+e")

  // Every opcode ends with the overlapped fetch of the next one.
  private val fetch = MCycleSpec.fetch(PC).assign(PC -> PC_INC)
  private val imm = read(PC).assign(PC -> PC_INC)

  val ops = Seq(
    OpSpec("NOP", 0x00, Seq(fetch)),
    OpSpec("LD HL,nn", 0x21, Seq(imm.assign(L -> RDATA), imm.assign(H -> RDATA), fetch)),
    OpSpec("LD A,n", 0x3E, Seq(imm.assign(Z -> RDATA), fetch.assign(A -> Z))),
    OpSpec("LD (HL),A", 0x77, Seq(write(HL, A), fetch)),
    OpSpec("LD A,(HL)", 0x7E, Seq(read(HL).assign(Z -> RDATA), fetch.assign(A -> Z))),
    OpSpec("INC A", 0x3C, Seq(fetch.assign(A -> A_INC, ZF -> A_INC_ZERO))),
    OpSpec("INC HL", 0x23, Seq(idle.assign(HL -> HL_INC), fetch)),
    OpSpec("JR NZ,e", 0x20, Seq(imm.assign(Z -> RDATA).branchIf(ZF.isSet, 2), idle.assign(PC -> PC_E), fetch)))

  val names = ops.map(o => o.opcode -> o.name).toMap

  /** The ISA plugin: contributes the opcode specs and the few adders they name. */
  class IsaPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()

      ops.foreach(seq.addOp)
      val pc = rf.read(PC)
      val aInc = rf.read(A) + 1
      seq.addSource(PC_INC, pc + 1)
      seq.addSource(HL_INC, rf.read(HL) + 1)
      seq.addSource(A_INC, aInc)
      seq.addSource(A_INC_ZERO, (aInc === 0).asUInt)
      seq.addSource(PC_E, (pc.asSInt + rf.read(Z).asSInt).asUInt)
      retainer.release()
    }
  }

  /** A second bus master standing in for DMA: while `steal` is high it reads FFFF and wins the bus. */
  class StealPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val bus = host[BusService]
      val retainer = retains(bus.busLock)
      awaitBuild()
      val steal = in Bool()
      val m = bus.newMaster(priority = 1)
      m.cmd.valid := steal
      m.cmd.write := False
      m.cmd.address := 0xFFFF
      m.cmd.data := 0
      retainer.release()
    }
  }

  def plugins(steal: Boolean) = Seq(
    new BusPlugin(RetroBusConfig(16, 8)),
    new SequencerPlugin(illegal = fetch),
    new RegFilePlugin(regFile),
    new IsaPlugin,
    new TracePlugin
  ) ++ (if (steal) Seq(new StealPlugin) else Nil)

  def apply(steal: Boolean = false) = RetroCore(plugins(steal)).setDefinitionName("Toy8")
}
