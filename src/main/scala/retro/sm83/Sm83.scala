package retro.sm83

import retro.core._
import spinal.core._
import spinal.lib.misc.plugin._

/** Sharp SM83 (Game Boy CPU), described as data on top of `retro.core`.
  *
  * Implemented so far: NOP; LD r,r' / r,n / r,(HL) / (HL),r / (HL),n; LD rr,nn; LD (BC|DE),A and
  * A,(BC|DE); LD (HL+/-),A and A,(HL+/-); INC/DEC r, (HL), rr; the 8-bit ALU on r, (HL) and n;
  * JR e, JR cc,e, JP nn. Every opcode has its documented M-cycle count and bus activity. */
object Sm83 {
  val regFile = RegFileSpec(
    regs = Seq("A", "F", "B", "C", "D", "E", "H", "L", "SPH", "SPL", "W", "Z").map(RegSpec(_, 8)) :+ RegSpec("PC", 16),
    pairs = Seq(PairSpec("BC", "B", "C"), PairSpec("DE", "D", "E"), PairSpec("HL", "H", "L"),
                PairSpec("SP", "SPH", "SPL"), PairSpec("WZ", "W", "Z")),
    flags = Seq(FlagSpec("ZF", "F", 7), FlagSpec("NF", "F", 6), FlagSpec("HF", "F", 5), FlagSpec("CF", "F", 4)))

  /** Registers the reference model also has (W and Z are internal temporaries). */
  val architectural = Seq("A", "F", "B", "C", "D", "E", "H", "L", "SPH", "SPL", "PC")

  /** The 3-bit r field of the encoding. Index 6 is (HL). */
  val r8 = Seq("B", "C", "D", "E", "H", "L", "(HL)", "A")
  val r16 = Seq("BC", "DE", "HL", "SP")

  object Alu {
    val ADD = 0; val ADC = 1; val SUB = 2; val SBC = 3; val AND = 4; val XOR = 5; val OR = 6; val CP = 7
    val INC = 8; val DEC = 9
    val names = Seq("ADD", "ADC", "SUB", "SBC", "AND", "XOR", "OR", "CP")
  }

  /** The 8-bit ALU. Operand A is the accumulator, operand B is picked by ALU_SRC using the
    * r encoding (6 selects Z, where (HL) and immediate operands are staged). Publishes the
    * sources "alu" (result) and "alu.F" (new flags; INC/DEC keep C). */
  class AluPlugin extends FiberPlugin {
    var op, src: CtrlKey = null

    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      op = seq.newKey("ALU_OP", 4)
      src = seq.newKey("ALU_SRC", 3)

      val a = rf.read("A")
      val b = Vec(r8.map(n => rf.read(if (n == "(HL)") "Z" else n)))(src.value)
      val cf = rf.flag("CF")
      val carryIn = (op.value === Alu.ADC || op.value === Alu.SBC) && cf

      val sum = a.resize(9) + b.resize(9) + carryIn.asUInt.resize(9)
      val sumH = (a(3 downto 0).resize(5) + b(3 downto 0).resize(5) + carryIn.asUInt.resize(5))(4)
      val diff = a.resize(9) - b.resize(9) - carryIn.asUInt.resize(9)
      val diffH = (a(3 downto 0).resize(5) - b(3 downto 0).resize(5) - carryIn.asUInt.resize(5))(4)

      val calc = UInt(8 bits)
      val n, h, c = Bool()
      n := False; h := False; c := False
      calc := 0
      switch(op.value) {
        is(Alu.ADD, Alu.ADC) { calc := sum(7 downto 0); h := sumH; c := sum(8) }
        is(Alu.SUB, Alu.SBC, Alu.CP) { calc := diff(7 downto 0); n := True; h := diffH; c := diff(8) }
        is(Alu.AND) { calc := a & b; h := True }
        is(Alu.XOR) { calc := a ^ b }
        is(Alu.OR) { calc := a | b }
        is(Alu.INC) { calc := b + 1; h := b(3 downto 0) === 0xF; c := cf }
        is(Alu.DEC) { calc := b - 1; n := True; h := b(3 downto 0) === 0; c := cf }
      }
      seq.addSource("alu", calc)
      seq.addSource("alu.F", ((calc === 0) ## n ## h ## c ## B"0000").asUInt)
      retainer.release()
    }
  }

  /** The instruction set as data, plus the incrementer/decrementer (IDU) and the JR adder. */
  class IsaPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val alu = host[AluPlugin]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      alu.logic.await() // the ALU keys must exist before the opcode table can name them

      for (p <- r16 :+ "PC") {
        seq.addSource(s"$p+1", rf.read(p) + 1)
        seq.addSource(s"$p-1", rf.read(p) - 1)
      }
      seq.addSource("PC+e", (rf.read("PC").asSInt + rf.read("Z").asSInt).asUInt)
      opcodes(alu.op, alu.src).foreach(seq.addOp)
      retainer.release()
    }
  }

  def opcodes(aluOp: CtrlKey, aluSrc: CtrlKey): Seq[OpSpec] = {
    val fetch = MCycle.fetch("PC").assign("PC" -> "PC+1")
    val imm = MCycle.read("PC").assign("PC" -> "PC+1")
    def alu(c: MCycle, op: Int, src: Int) = c.set(aluOp, op).set(aluSrc, src)
    val ops = Seq.newBuilder[OpSpec]
    def op(name: String, opcode: Int, cycles: MCycle*) = ops += OpSpec(name, opcode, cycles)

    op("NOP", 0x00, fetch)

    for (d <- 0 to 7; s <- 0 to 7 if !(d == 6 && s == 6)) {
      val name = s"LD ${r8(d)},${r8(s)}"
      val code = 0x40 | d << 3 | s
      if (s == 6) op(name, code, MCycle.read("HL").assign(r8(d) -> "rdata"), fetch)
      else if (d == 6) op(name, code, MCycle.write("HL", r8(s)), fetch)
      else op(name, code, fetch.assign(r8(d) -> r8(s)))
    }
    for (d <- 0 to 7) {
      val code = 0x06 | d << 3
      if (d == 6) op("LD (HL),n", code, imm.assign("Z" -> "rdata"), MCycle.write("HL", "Z"), fetch)
      else op(s"LD ${r8(d)},n", code, imm.assign(r8(d) -> "rdata"), fetch)
    }
    for ((rr, i) <- r16.zipWithIndex) {
      op(s"LD $rr,nn", 0x01 | i << 4, imm.assign(lo(rr) -> "rdata"), imm.assign(hi(rr) -> "rdata"), fetch)
      op(s"INC $rr", 0x03 | i << 4, MCycle.idle.assign(rr -> s"$rr+1"), fetch)
      op(s"DEC $rr", 0x0B | i << 4, MCycle.idle.assign(rr -> s"$rr-1"), fetch)
    }
    op("LD (BC),A", 0x02, MCycle.write("BC", "A"), fetch)
    op("LD (DE),A", 0x12, MCycle.write("DE", "A"), fetch)
    op("LD A,(BC)", 0x0A, MCycle.read("BC").assign("A" -> "rdata"), fetch)
    op("LD A,(DE)", 0x1A, MCycle.read("DE").assign("A" -> "rdata"), fetch)
    op("LD (HL+),A", 0x22, MCycle.write("HL", "A").assign("HL" -> "HL+1"), fetch)
    op("LD (HL-),A", 0x32, MCycle.write("HL", "A").assign("HL" -> "HL-1"), fetch)
    op("LD A,(HL+)", 0x2A, MCycle.read("HL").assign("A" -> "rdata", "HL" -> "HL+1"), fetch)
    op("LD A,(HL-)", 0x3A, MCycle.read("HL").assign("A" -> "rdata", "HL" -> "HL-1"), fetch)

    for (r <- 0 to 7; (name, aop, code) <- Seq(("INC", Alu.INC, 0x04 | r << 3), ("DEC", Alu.DEC, 0x05 | r << 3))) {
      if (r == 6) op(s"$name (HL)", code, MCycle.read("HL").assign("Z" -> "rdata"),
        alu(MCycle.write("HL", "alu").assign("F" -> "alu.F"), aop, 6), fetch)
      else op(s"$name ${r8(r)}", code, alu(fetch.assign(r8(r) -> "alu", "F" -> "alu.F"), aop, r))
    }
    for (o <- 0 to 7) {
      // CP only updates the flags.
      def writeBack(c: MCycle) = if (o == Alu.CP) c.assign("F" -> "alu.F") else c.assign("A" -> "alu", "F" -> "alu.F")
      for (r <- 0 to 7) {
        val code = 0x80 | o << 3 | r
        val name = s"${Alu.names(o)} A,${r8(r)}"
        if (r == 6) op(name, code, MCycle.read("HL").assign("Z" -> "rdata"), alu(writeBack(fetch), o, 6))
        else op(name, code, alu(writeBack(fetch), o, r))
      }
      op(s"${Alu.names(o)} A,n", 0xC6 | o << 3, imm.assign("Z" -> "rdata"), alu(writeBack(fetch), o, 6))
    }

    op("JR e", 0x18, imm.assign("Z" -> "rdata"), MCycle.idle.assign("PC" -> "PC+e"), fetch)
    // Not taken: skip the adder cycle straight to the fetch.
    for ((cc, skipIf, code) <- Seq(("NZ", "ZF", 0x20), ("Z", "!ZF", 0x28), ("NC", "CF", 0x30), ("C", "!CF", 0x38)))
      op(s"JR $cc,e", code, imm.assign("Z" -> "rdata").branchIf(skipIf, 2), MCycle.idle.assign("PC" -> "PC+e"), fetch)
    op("JP nn", 0xC3, imm.assign("Z" -> "rdata"), imm.assign("W" -> "rdata"), MCycle.idle.assign("PC" -> "WZ"), fetch)

    ops.result()
  }
  private def lo(rr: String) = if (rr == "SP") "SPL" else rr.substring(1, 2)
  private def hi(rr: String) = if (rr == "SP") "SPH" else rr.substring(0, 1)

  /** Mnemonics by opcode, for traces. */
  lazy val names: Map[Int, String] = opcodes(new CtrlKey("op", 4, 0), new CtrlKey("src", 3, 0)).map(o => o.opcode -> o.name).toMap

  def plugins(withTrace: Boolean = true): Seq[Hostable] = Seq(
    new BusPlugin(RetroBusConfig(16, 8)),
    new SequencerPlugin(),
    new RegFilePlugin(regFile),
    new AluPlugin,
    new IsaPlugin
  ) ++ (if (withTrace) Seq(new TracePlugin) else Nil)
}

object Sm83Core {
  def apply(withTrace: Boolean = true): RetroCore = RetroCore(Sm83.plugins(withTrace)).setDefinitionName("Sm83Core")
}

object Sm83Verilog extends App {
  SpinalConfig(targetDirectory = "gen").generateVerilog(Sm83Core(withTrace = false))
}
