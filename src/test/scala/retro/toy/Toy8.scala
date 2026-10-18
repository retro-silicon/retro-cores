package retro.toy

import retro.core._
import spinal.core._
import spinal.lib.misc.plugin._

/** A toy 8-bit CPU with SM83-like encodings and timings, used only to test the framework. */
object Toy8 {
  val regFile = RegFileSpec(
    regs = Seq(RegSpec("A", 8), RegSpec("F", 8), RegSpec("H", 8), RegSpec("L", 8),
               RegSpec("W", 8), RegSpec("Z", 8), RegSpec("PC", 16)),
    pairs = Seq(PairSpec("HL", "H", "L"), PairSpec("WZ", "W", "Z")),
    flags = Seq(FlagSpec("ZF", "F", 7)))

  // Every opcode ends with the overlapped fetch of the next one.
  private def fetch = MCycle.fetch("PC").assign("PC" -> "PC+1")
  private def imm = MCycle.read("PC").assign("PC" -> "PC+1")

  val ops = Seq(
    OpSpec("NOP", 0x00, Seq(fetch)),
    OpSpec("LD HL,nn", 0x21, Seq(imm.assign("L" -> "rdata"), imm.assign("H" -> "rdata"), fetch)),
    OpSpec("LD A,n", 0x3E, Seq(imm.assign("Z" -> "rdata"), fetch.assign("A" -> "Z"))),
    OpSpec("LD (HL),A", 0x77, Seq(MCycle.write("HL", "A"), fetch)),
    OpSpec("LD A,(HL)", 0x7E, Seq(MCycle.read("HL").assign("Z" -> "rdata"), fetch.assign("A" -> "Z"))),
    OpSpec("INC A", 0x3C, Seq(fetch.assign("A" -> "A+1", "ZF" -> "zero(A+1)"))),
    OpSpec("INC HL", 0x23, Seq(MCycle.idle.assign("HL" -> "HL+1"), fetch)),
    OpSpec("JR NZ,e", 0x20, Seq(imm.assign("Z" -> "rdata").branchIf("ZF", 2), MCycle.idle.assign("PC" -> "PC+e"), fetch)))

  val names = ops.map(o => o.opcode -> o.name).toMap

  /** The ISA plugin: contributes the opcode specs and the few adders they name. */
  class IsaPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()

      ops.foreach(seq.addOp)
      val pc = rf.read("PC")
      val aInc = rf.read("A") + 1
      seq.addSource("PC+1", pc + 1)
      seq.addSource("HL+1", rf.read("HL") + 1)
      seq.addSource("A+1", aInc)
      seq.addSource("zero(A+1)", (aInc === 0).asUInt)
      seq.addSource("PC+e", (pc.asSInt + rf.read("Z").asSInt).asUInt)
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
    new SequencerPlugin(),
    new RegFilePlugin(regFile),
    new IsaPlugin,
    new TracePlugin
  ) ++ (if (steal) Seq(new StealPlugin) else Nil)

  def apply(steal: Boolean = false) = RetroCore(plugins(steal)).setDefinitionName("Toy8")
}
