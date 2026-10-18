package retro.sm83

import retro.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin._

/** Sharp SM83 (Game Boy CPU), described as data on top of `retro.core`.
  *
  * The full documented opcode set is implemented, CB-prefixed ops included, with interrupts,
  * EI/DI/RETI, HALT and the HALT bug. STOP and the eleven unused opcodes behave as a 1 M-cycle
  * NOP (the real chip enters low-power mode or locks up). Opcode space is 10 bits wide:
  * 0x000-0x0FF base opcodes, 0x100-0x1FF CB xx, 0x200 the interrupt dispatch sequence. */
object Sm83 {
  val regFile = RegFileSpec(
    regs = Seq("A", "F", "B", "C", "D", "E", "H", "L", "SPH", "SPL", "W", "Z").map(RegSpec(_, 8)) :+ RegSpec("PC", 16),
    pairs = Seq(PairSpec("AF", "A", "F"), PairSpec("BC", "B", "C"), PairSpec("DE", "D", "E"), PairSpec("HL", "H", "L"),
                PairSpec("SP", "SPH", "SPL"), PairSpec("WZ", "W", "Z")),
    flags = Seq(FlagSpec("ZF", "F", 7), FlagSpec("NF", "F", 6), FlagSpec("HF", "F", 5), FlagSpec("CF", "F", 4)))

  /** Registers the reference model also has (W and Z are internal temporaries). */
  val architectural = Seq("A", "F", "B", "C", "D", "E", "H", "L", "SPH", "SPL", "PC")

  /** The 3-bit r field of the encoding. Index 6 is (HL); ALU and shifter read Z there. */
  val r8 = Seq("B", "C", "D", "E", "H", "L", "(HL)", "A")
  val r16 = Seq("BC", "DE", "HL", "SP")
  val r16Stack = Seq("BC", "DE", "HL", "AF")
  /** Condition codes: name, and the flag test that makes the condition fail. */
  val conditions = Seq(("NZ", "ZF"), ("Z", "!ZF"), ("NC", "CF"), ("C", "!CF"))

  val CB_PAGE = 0x100
  val DISPATCH = 0x200

  object Alu {
    val ADD = 0; val ADC = 1; val SUB = 2; val SBC = 3; val AND = 4; val XOR = 5; val OR = 6; val CP = 7
    val INC = 8; val DEC = 9; val DAA = 10; val CPL = 11; val SCF = 12; val CCF = 13
    val names = Seq("ADD", "ADC", "SUB", "SBC", "AND", "XOR", "OR", "CP")
  }
  object Shift {
    val RLC = 0; val RRC = 1; val RL = 2; val RR = 3; val SLA = 4; val SRA = 5; val SWAP = 6; val SRL = 7
    val BIT = 8; val RES = 9; val SET = 10
    val names = Seq("RLC", "RRC", "RL", "RR", "SLA", "SRA", "SWAP", "SRL")
  }
  object Ime { val NONE = 0; val DI = 1; val EI = 2; val RETI = 3 }

  /** The control keys the opcode table sets, owned by the ALU, shifter and interrupt plugins. */
  case class Keys(aluOp: CtrlKey, aluSrc: CtrlKey, shOp: CtrlKey, shSrc: CtrlKey, shBit: CtrlKey, imeOp: CtrlKey)

  private def operands(rf: RegFileService) = Vec(r8.map(n => rf.read(if (n == "(HL)") "Z" else n)))

  /** The 8-bit ALU plus the 16-bit adds. Operand A is the accumulator, operand B is picked by
    * ALU_SRC using the r encoding. Publishes "alu" and "alu.F" (new flags), "HL+rr" and
    * "F(HL+rr)" for ADD HL,rr, and "SP+e" and "F(SP+e)" for ADD SP,e and LD HL,SP+e. */
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
      val b = operands(rf)(src.value)
      val (zf, nf, hf, cf) = (rf.flag("ZF"), rf.flag("NF"), rf.flag("HF"), rf.flag("CF"))
      val carryIn = (op.value === Alu.ADC || op.value === Alu.SBC) && cf

      val sum = a.resize(9) + b.resize(9) + carryIn.asUInt.resize(9)
      val sumH = (a(3 downto 0).resize(5) + b(3 downto 0).resize(5) + carryIn.asUInt.resize(5))(4)
      val diff = a.resize(9) - b.resize(9) - carryIn.asUInt.resize(9)
      val diffH = (a(3 downto 0).resize(5) - b(3 downto 0).resize(5) - carryIn.asUInt.resize(5))(4)
      val daaLo = hf || (!nf && a(3 downto 0) > 9)
      val daaHi = cf || (!nf && a > 0x99)
      val daaCorr = (daaHi ? U(0x60, 8 bits) | U(0, 8 bits)) | (daaLo ? U(0x06, 8 bits) | U(0, 8 bits))

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
        is(Alu.DAA) { calc := nf ? (a - daaCorr) | (a + daaCorr); n := nf; c := daaHi }
        is(Alu.CPL) { calc := ~a; n := True; h := True; c := cf }
        is(Alu.SCF) { calc := a; c := True }
        is(Alu.CCF) { calc := a; c := !cf }
      }
      val keepZ = op.value === Alu.CPL || op.value === Alu.SCF || op.value === Alu.CCF
      val z = keepZ ? zf | (calc === 0)
      seq.addSource("alu", calc)
      seq.addSource("alu.F", (z ## n ## h ## c ## B"0000").asUInt)

      val hl = rf.read("HL")
      for (rr <- r16) {
        val v = rf.read(rr)
        val s = hl.resize(17) + v.resize(17)
        val h12 = (hl(11 downto 0).resize(13) + v(11 downto 0).resize(13))(12)
        seq.addSource(s"HL+$rr", s(15 downto 0))
        seq.addSource(s"F(HL+$rr)", (zf ## False ## h12 ## s(16) ## B"0000").asUInt)
      }
      val sp = rf.read("SP")
      val e = rf.read("Z")
      seq.addSource("SP+e", (sp.asSInt + e.asSInt).asUInt)
      val spH = (sp(3 downto 0).resize(5) + e(3 downto 0).resize(5))(4)
      val spC = (sp(7 downto 0).resize(9) + e.resize(9))(8)
      seq.addSource("F(SP+e)", (B"00" ## spH ## spC ## B"0000").asUInt)
      retainer.release()
    }
  }

  /** Rotates, shifts, SWAP and the bit ops (CB page, plus RLCA/RRCA/RLA/RRA). Operand picked by
    * SH_SRC with the r encoding. Publishes "sh", "sh.F" and "sh.F.z0" (Z forced to 0). */
  class ShifterPlugin extends FiberPlugin {
    var op, src, bit: CtrlKey = null

    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      op = seq.newKey("SH_OP", 4)
      src = seq.newKey("SH_SRC", 3)
      bit = seq.newKey("SH_BIT", 3)

      val v = operands(rf)(src.value)
      val cf = rf.flag("CF")
      val mask = (U(1, 8 bits) |<< bit.value)
      val res = UInt(8 bits)
      val carryOut = Bool()
      res := v
      carryOut := False
      switch(op.value) {
        is(Shift.RLC) { res := v.rotateLeft(1); carryOut := v(7) }
        is(Shift.RRC) { res := v.rotateRight(1); carryOut := v(0) }
        is(Shift.RL) { res := (v(6 downto 0) ## cf).asUInt; carryOut := v(7) }
        is(Shift.RR) { res := (cf ## v(7 downto 1)).asUInt; carryOut := v(0) }
        is(Shift.SLA) { res := v |<< 1; carryOut := v(7) }
        is(Shift.SRA) { res := (v(7) ## v(7 downto 1)).asUInt; carryOut := v(0) }
        is(Shift.SWAP) { res := (v(3 downto 0) ## v(7 downto 4)).asUInt }
        is(Shift.SRL) { res := v |>> 1; carryOut := v(0) }
        is(Shift.RES) { res := v & ~mask }
        is(Shift.SET) { res := v | mask }
      }
      val isBit = op.value === Shift.BIT
      val flags = isBit ? (((v & mask) === 0) ## False ## True ## cf) | ((res === 0) ## False ## False ## carryOut)
      seq.addSource("sh", res)
      seq.addSource("sh.F", (flags ## B"0000").asUInt)
      seq.addSource("sh.F.z0", (False ## flags(2 downto 0) ## B"0000").asUInt)
      retainer.release()
    }
  }

  /** IME, EI/DI/RETI and interrupt dispatch. `requests` is IE & IF from the SoC (bit 0 VBlank
    * ... bit 4 Joypad); `ack` pulses the index being serviced so the SoC can clear its IF bit.
    *
    * Interrupts are taken at an instruction-ending fetch: the fetched opcode is discarded and
    * IR loads the dispatch opcode (0x200), which pushes PC and jumps to 0x40 + 8 * index.
    * EI takes effect after the following instruction; DI and RETI immediately. */
  class InterruptPlugin extends FiberPlugin {
    var imeOp: CtrlKey = null

    val logic = during setup new Area {
      val seq = host[SequencerService]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      imeOp = seq.newKey("IME_OP", 2)

      val requests = in Bits (5 bits)
      val ack = master(Flow(UInt(3 bits)))
      val ime = RegInit(False).simPublic()
      val imeNext = RegInit(False)
      val vector = Reg(UInt(3 bits)) init 0

      val pending = requests =/= 0
      val enabled = ime || imeNext
      val effective = imeOp.value === Ime.RETI || (enabled && imeOp.value =/= Ime.DI)
      val dispatch = effective && pending
      val index = OHToUInt(OHMasking.first(requests))

      seq.addFetchOverride(dispatch, U(DISPATCH, 10 bits))
      when(seq.fetchFire) {
        ime := effective && !dispatch
        imeNext := imeOp.value === Ime.EI && !dispatch
        when(dispatch) { vector := index }
      }
      ack.valid := seq.fetchFire && dispatch
      ack.payload := index

      seq.addSource("INT.vector", U(0x40, 16 bits) + (vector @@ U"000").resized)
      seq.addCond("INT.pending", pending)
      seq.addCond("!INT.pending", !pending)
      seq.addCond("INT.haltbug", pending && !enabled)
      retainer.release()
    }
  }

  /** The opcode table, plus the IDU (16-bit inc/dec on the address side) and small constants. */
  class IsaPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val alu = host[AluPlugin]
      val sh = host[ShifterPlugin]
      val int = host[InterruptPlugin]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      // The keys must exist before the opcode table can name them.
      alu.logic.await(); sh.logic.await(); int.logic.await()

      for (p <- r16 :+ "PC" :+ "WZ") {
        seq.addSource(s"$p+1", rf.read(p) + 1)
        seq.addSource(s"$p-1", rf.read(p) - 1)
      }
      val pc = rf.read("PC")
      val z = rf.read("Z")
      seq.addSource("PC+e", (pc.asSInt + z.asSInt).asUInt)
      seq.addSource("PC.hi", pc(15 downto 8))
      seq.addSource("PC.lo", pc(7 downto 0))
      for (n <- 0 to 7) seq.addSource(s"RST$n", U(n * 8, 16 bits))
      seq.addSource("Z&F0", z & 0xF0)
      seq.addSource("FF00+Z", U(0xFF, 8 bits) @@ z)
      seq.addSource("FF00+C", U(0xFF, 8 bits) @@ rf.read("C"))
      opcodes(Keys(alu.op, alu.src, sh.op, sh.src, sh.bit, int.imeOp)).foreach(seq.addOp)
      retainer.release()
    }
  }

  def opcodes(k: Keys): Seq[OpSpec] = {
    val fetch = MCycle.fetch("PC").assign("PC" -> "PC+1")
    val imm = MCycle.read("PC").assign("PC" -> "PC+1")
    val idle = MCycle.idle
    def alu(c: MCycle, op: Int, src: Int = 7) = c.set(k.aluOp, op).set(k.aluSrc, src)
    def shift(c: MCycle, op: Int, src: Int, bit: Int = 0) = c.set(k.shOp, op).set(k.shSrc, src).set(k.shBit, bit)
    // Push PC (or a pair) in the SM83 order: SP-1, write high byte, SP-1, write low byte.
    def push(hi: String, lo: String, last: MCycle => MCycle = identity) = Seq(
      idle.assign("SP" -> "SP-1"), MCycle.write("SP", hi).assign("SP" -> "SP-1"), last(MCycle.write("SP", lo)))
    def pop(lo: String, hi: String) = Seq(
      MCycle.read("SP").assign(lo -> "rdata", "SP" -> "SP+1"), MCycle.read("SP").assign(hi -> "rdata", "SP" -> "SP+1"))
    val ops = Seq.newBuilder[OpSpec]
    def op(name: String, opcode: Int, cycles: MCycle*) = ops += OpSpec(name, opcode, cycles)
    def lo(rr: String) = if (rr == "SP") "SPL" else rr.substring(1, 2)
    def hi(rr: String) = if (rr == "SP") "SPH" else rr.substring(0, 1)

    // ---- 8-bit loads ---------------------------------------------------------------------
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
    op("LD (BC),A", 0x02, MCycle.write("BC", "A"), fetch)
    op("LD (DE),A", 0x12, MCycle.write("DE", "A"), fetch)
    op("LD A,(BC)", 0x0A, MCycle.read("BC").assign("A" -> "rdata"), fetch)
    op("LD A,(DE)", 0x1A, MCycle.read("DE").assign("A" -> "rdata"), fetch)
    op("LD (HL+),A", 0x22, MCycle.write("HL", "A").assign("HL" -> "HL+1"), fetch)
    op("LD (HL-),A", 0x32, MCycle.write("HL", "A").assign("HL" -> "HL-1"), fetch)
    op("LD A,(HL+)", 0x2A, MCycle.read("HL").assign("A" -> "rdata", "HL" -> "HL+1"), fetch)
    op("LD A,(HL-)", 0x3A, MCycle.read("HL").assign("A" -> "rdata", "HL" -> "HL-1"), fetch)
    op("LDH (n),A", 0xE0, imm.assign("Z" -> "rdata"), MCycle.write("FF00+Z", "A"), fetch)
    op("LDH A,(n)", 0xF0, imm.assign("Z" -> "rdata"), MCycle.read("FF00+Z").assign("A" -> "rdata"), fetch)
    op("LD (C),A", 0xE2, MCycle.write("FF00+C", "A"), fetch)
    op("LD A,(C)", 0xF2, MCycle.read("FF00+C").assign("A" -> "rdata"), fetch)
    op("LD (nn),A", 0xEA, imm.assign("Z" -> "rdata"), imm.assign("W" -> "rdata"), MCycle.write("WZ", "A"), fetch)
    op("LD A,(nn)", 0xFA, imm.assign("Z" -> "rdata"), imm.assign("W" -> "rdata"), MCycle.read("WZ").assign("A" -> "rdata"), fetch)

    // ---- 16-bit loads, arithmetic and stack ------------------------------------------------
    for ((rr, i) <- r16.zipWithIndex) {
      op(s"LD $rr,nn", 0x01 | i << 4, imm.assign(lo(rr) -> "rdata"), imm.assign(hi(rr) -> "rdata"), fetch)
      op(s"INC $rr", 0x03 | i << 4, idle.assign(rr -> s"$rr+1"), fetch)
      op(s"DEC $rr", 0x0B | i << 4, idle.assign(rr -> s"$rr-1"), fetch)
      op(s"ADD HL,$rr", 0x09 | i << 4, idle.assign("HL" -> s"HL+$rr", "F" -> s"F(HL+$rr)"), fetch)
    }
    op("LD (nn),SP", 0x08, imm.assign("Z" -> "rdata"), imm.assign("W" -> "rdata"),
      MCycle.write("WZ", "SPL").assign("WZ" -> "WZ+1"), MCycle.write("WZ", "SPH"), fetch)
    op("LD SP,HL", 0xF9, idle.assign("SP" -> "HL"), fetch)
    op("ADD SP,e", 0xE8, imm.assign("Z" -> "rdata"), idle.assign("SP" -> "SP+e", "F" -> "F(SP+e)"), idle, fetch)
    op("LD HL,SP+e", 0xF8, imm.assign("Z" -> "rdata"), idle.assign("HL" -> "SP+e", "F" -> "F(SP+e)"), fetch)
    for ((rr, i) <- r16Stack.zipWithIndex) {
      op(s"PUSH $rr", 0xC5 | i << 4, push(hi(rr), lo(rr)) :+ fetch: _*)
      if (rr == "AF") op("POP AF", 0xF1, MCycle.read("SP").assign("Z" -> "rdata", "SP" -> "SP+1"),
        MCycle.read("SP").assign("A" -> "rdata", "F" -> "Z&F0", "SP" -> "SP+1"), fetch)
      else op(s"POP $rr", 0xC1 | i << 4, pop(lo(rr), hi(rr)) :+ fetch: _*)
    }

    // ---- 8-bit ALU -------------------------------------------------------------------------
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
    for ((name, aop, code) <- Seq(("DAA", Alu.DAA, 0x27), ("CPL", Alu.CPL, 0x2F), ("SCF", Alu.SCF, 0x37), ("CCF", Alu.CCF, 0x3F)))
      op(name, code, alu(fetch.assign("A" -> "alu", "F" -> "alu.F"), aop))
    for ((name, sop, code) <- Seq(("RLCA", Shift.RLC, 0x07), ("RRCA", Shift.RRC, 0x0F), ("RLA", Shift.RL, 0x17), ("RRA", Shift.RR, 0x1F)))
      op(name, code, shift(fetch.assign("A" -> "sh", "F" -> "sh.F.z0"), sop, 7))

    // ---- Control flow ----------------------------------------------------------------------
    op("JR e", 0x18, imm.assign("Z" -> "rdata"), idle.assign("PC" -> "PC+e"), fetch)
    op("JP nn", 0xC3, imm.assign("Z" -> "rdata"), imm.assign("W" -> "rdata"), idle.assign("PC" -> "WZ"), fetch)
    op("JP HL", 0xE9, MCycle.fetch("HL").assign("PC" -> "HL+1"))
    op("CALL nn", 0xCD, imm.assign("Z" -> "rdata") +: imm.assign("W" -> "rdata") +:
      push("PC.hi", "PC.lo", _.assign("PC" -> "WZ")) :+ fetch: _*)
    op("RET", 0xC9, pop("Z", "W") :+ idle.assign("PC" -> "WZ") :+ fetch: _*)
    op("RETI", 0xD9, pop("Z", "W") :+ idle.assign("PC" -> "WZ") :+ fetch.set(k.imeOp, Ime.RETI): _*)
    for (n <- 0 to 7) op(f"RST ${n * 8}%02XH", 0xC7 | n << 3, push("PC.hi", "PC.lo", _.assign("PC" -> s"RST$n")) :+ fetch: _*)
    // Conditional forms branch to their final fetch when the condition fails.
    for (((cc, fail), i) <- conditions.zipWithIndex) {
      op(s"JR $cc,e", 0x20 | i << 3, imm.assign("Z" -> "rdata").branchIf(fail, 2), idle.assign("PC" -> "PC+e"), fetch)
      op(s"JP $cc,nn", 0xC2 | i << 3, imm.assign("Z" -> "rdata"), imm.assign("W" -> "rdata").branchIf(fail, 3),
        idle.assign("PC" -> "WZ"), fetch)
      op(s"CALL $cc,nn", 0xC4 | i << 3, imm.assign("Z" -> "rdata") +: imm.assign("W" -> "rdata").branchIf(fail, 5) +:
        push("PC.hi", "PC.lo", _.assign("PC" -> "WZ")) :+ fetch: _*)
      op(s"RET $cc", 0xC0 | i << 3, idle.branchIf(fail, 4) +: pop("Z", "W") :+ idle.assign("PC" -> "WZ") :+ fetch: _*)
    }

    // ---- Interrupt control ------------------------------------------------------------------
    op("DI", 0xF3, fetch.set(k.imeOp, Ime.DI))
    op("EI", 0xFB, fetch.set(k.imeOp, Ime.EI))
    // HALT waits for a pending interrupt. Entered with one already pending and IME off, the
    // next fetch does not increment PC (the HALT bug).
    op("HALT", 0x76, idle.branchIf("INT.haltbug", 3), idle.branchIf("!INT.pending", 1), fetch, MCycle.fetch("PC"))
    op("INT", DISPATCH, idle.assign("PC" -> "PC-1") +: push("PC.hi", "PC.lo", _.assign("PC" -> "INT.vector")) :+ fetch: _*)

    // ---- CB page -----------------------------------------------------------------------------
    op("PREFIX CB", 0xCB, MCycle.prefix("PC", 1).assign("PC" -> "PC+1"))
    for (x <- 0 to 255) {
      val (r, y, group) = (x & 7, (x >> 3) & 7, x >> 6)
      val code = CB_PAGE | x
      val (name, sop, bit, writes, flags) = group match {
        case 0 => (s"${Shift.names(y)} ${r8(r)}", y, 0, true, true)
        case 1 => (s"BIT $y,${r8(r)}", Shift.BIT, y, false, true)
        case 2 => (s"RES $y,${r8(r)}", Shift.RES, y, true, false)
        case 3 => (s"SET $y,${r8(r)}", Shift.SET, y, true, false)
      }
      def result(c: MCycle) = if (flags) c.assign("F" -> "sh.F") else c
      if (r == 6) {
        val read = MCycle.read("HL").assign("Z" -> "rdata")
        if (writes) op(name, code, read, shift(result(MCycle.write("HL", "sh")), sop, 6, bit), fetch)
        else op(name, code, read, shift(result(fetch), sop, 6, bit))
      } else op(name, code, shift(result(if (writes) fetch.assign(r8(r) -> "sh") else fetch), sop, r, bit))
    }
    ops.result()
  }

  /** Mnemonics by opcode (CB xx as 0x1xx), for traces. */
  lazy val names: Map[Int, String] = {
    def key(n: String) = new CtrlKey(n, 4, 0)
    opcodes(Keys(key("a"), key("b"), key("c"), key("d"), key("e"), key("f"))).map(o => o.opcode -> o.name).toMap
  }

  def plugins(withTrace: Boolean = true): Seq[Hostable] = Seq(
    new BusPlugin(RetroBusConfig(16, 8)),
    new SequencerPlugin(opcodeWidth = 10),
    new RegFilePlugin(regFile),
    new AluPlugin,
    new ShifterPlugin,
    new InterruptPlugin,
    new IsaPlugin
  ) ++ (if (withTrace) Seq(new TracePlugin) else Nil)
}

object Sm83Core {
  def apply(withTrace: Boolean = true): RetroCore = RetroCore(Sm83.plugins(withTrace)).setDefinitionName("Sm83Core")
}

object Sm83Verilog extends App {
  SpinalConfig(targetDirectory = "gen").generateVerilog(Sm83Core(withTrace = false))
}
