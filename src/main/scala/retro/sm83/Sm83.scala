package retro.sm83

import retro.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin._

/** SM83 registers, pairs and flags. W and Z are the internal temporaries; Z also stages (HL) and
  * immediate operands for the ALU and the shifter. */
object Sm83Regs {
  val A = Register("A", 8); val F = Register("F", 8)
  val B = Register("B", 8); val C = Register("C", 8)
  val D = Register("D", 8); val E = Register("E", 8)
  val H = Register("H", 8); val L = Register("L", 8)
  val SPH = Register("SPH", 8); val SPL = Register("SPL", 8)
  val W = Register("W", 8); val Z = Register("Z", 8)
  val PC = Register("PC", 16)

  val AF = Pair("AF", A, F); val BC = Pair("BC", B, C); val DE = Pair("DE", D, E)
  val HL = Pair("HL", H, L); val SP = Pair("SP", SPH, SPL); val WZ = Pair("WZ", W, Z)

  val ZF = Flag("ZF", F, 7); val NF = Flag("NF", F, 6); val HF = Flag("HF", F, 5); val CF = Flag("CF", F, 4)

  val spec = RegFileSpec(Seq(A, F, B, C, D, E, H, L, SPH, SPL, W, Z, PC), Seq(AF, BC, DE, HL, SP, WZ), Seq(ZF, NF, HF, CF))

  /** Registers the reference model also has. */
  val architectural = Seq(A, F, B, C, D, E, H, L, SPH, SPL, PC)

  /** The 3-bit r field of the encoding. Index 6 is (HL); there the operand is staged in Z. */
  val r8 = IndexedSeq(B, C, D, E, H, L, Z, A)
  val r8Names = IndexedSeq("B", "C", "D", "E", "H", "L", "(HL)", "A")
  val r16 = IndexedSeq(BC, DE, HL, SP)
  val r16Stack = IndexedSeq(BC, DE, HL, AF)
  /** Condition codes in encoding order: name, and the flag test under which the condition fails. */
  val conditions = IndexedSeq(("NZ", ZF.isSet), ("Z", ZF.isClear), ("NC", CF.isSet), ("C", CF.isClear))
}
import Sm83Regs._

/** The 16-bit incrementer/decrementer on the address side. */
object Idu {
  val pairs: Seq[Storage] = Seq(BC, DE, HL, SP, WZ, PC)
  val inc: Map[Storage, Value] = pairs.map(p => p -> Value(s"$p+1")).toMap
  val dec: Map[Storage, Value] = pairs.map(p => p -> Value(s"$p-1")).toMap
}

/** The 8-bit ALU and the 16-bit adds. */
object Alu {
  val OP = CtrlKey("ALU_OP", 4)
  /** Operand B, using the r encoding (6 selects Z). Operand A is always the accumulator. */
  val SRC = CtrlKey("ALU_SRC", 3)
  val RESULT = Value("alu")
  val FLAGS = Value("alu.F")
  val ADD_HL: Map[Pair, Value] = r16.map(rr => rr -> Value(s"HL+$rr")).toMap
  val ADD_HL_FLAGS: Map[Pair, Value] = r16.map(rr => rr -> Value(s"F(HL+$rr)")).toMap
  /** SP plus the signed byte in Z, for ADD SP,e and LD HL,SP+e. */
  val SP_E = Value("SP+e")
  val SP_E_FLAGS = Value("F(SP+e)")

  val ADD = 0; val ADC = 1; val SUB = 2; val SBC = 3; val AND = 4; val XOR = 5; val OR = 6; val CP = 7
  val INC = 8; val DEC = 9; val DAA = 10; val CPL = 11; val SCF = 12; val CCF = 13
  val names = Seq("ADD", "ADC", "SUB", "SBC", "AND", "XOR", "OR", "CP")
}

/** Rotates, shifts, SWAP and the bit ops (CB page, plus RLCA/RRCA/RLA/RRA). */
object Shifter {
  val OP = CtrlKey("SH_OP", 4)
  val SRC = CtrlKey("SH_SRC", 3)
  val BIT = CtrlKey("SH_BIT", 3)
  val RESULT = Value("sh")
  val FLAGS = Value("sh.F")
  /** Flags with Z forced to 0, for the accumulator rotates. */
  val FLAGS_Z0 = Value("sh.F.z0")

  val RLC = 0; val RRC = 1; val RL = 2; val RR = 3; val SLA = 4; val SRA = 5; val SWAP = 6; val SRL = 7
  val TEST = 8; val RES = 9; val SET = 10
  val names = Seq("RLC", "RRC", "RL", "RR", "SLA", "SRA", "SWAP", "SRL")
}

/** IME control and interrupt dispatch. */
object Interrupts {
  /** How an instruction's final fetch treats IME. */
  val IME_OP = CtrlKey("IME_OP", 2)
  val NONE = 0; val DI = 1; val EI = 2; val RETI = 3

  val VECTOR = Value("INT.vector")
  val PENDING = Cond("INT.pending")
  val NOT_PENDING = Cond("!INT.pending")
  /** An interrupt is pending while IME is off: HALT exits without incrementing PC. */
  val HALT_BUG = Cond("INT.haltbug")
}

/** Small values the opcode table needs that belong to no other unit. */
object Isa {
  /** PC plus the signed byte in Z, for JR. */
  val PC_E = Value("PC+e")
  val PC_HI = Value("PC.hi")
  val PC_LO = Value("PC.lo")
  val RST = (0 to 7).map(n => Value(f"RST${n * 8}%02XH"))
  /** Z with the low nibble cleared, for POP AF. */
  val Z_F0 = Value("Z&F0")
  val HIGH_Z = Value("FF00+Z")
  val HIGH_C = Value("FF00+C")
}

/** Sharp SM83 (Game Boy CPU), described as data on top of `retro.core`.
  *
  * The full documented opcode set is implemented, CB-prefixed ops included, with interrupts,
  * EI/DI/RETI, HALT and the HALT bug. STOP and the eleven unused opcodes behave as a 1 M-cycle
  * NOP (the real chip enters low-power mode or locks up). Opcode space is 10 bits wide:
  * 0x000-0x0FF base opcodes, 0x100-0x1FF CB xx, 0x200 the interrupt dispatch sequence. */
object Sm83 {
  val CB_PAGE = 0x100
  val DISPATCH = 0x200

  private def operand(rf: RegFileService, sel: UInt) = Vec(r8.map(rf.read))(sel)

  class AluPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      seq.addKey(Alu.OP)
      seq.addKey(Alu.SRC)
      val op = seq.ctrl(Alu.OP)

      val a = rf.read(A)
      val b = operand(rf, seq.ctrl(Alu.SRC))
      val (zf, nf, hf, cf) = (rf.flag(ZF), rf.flag(NF), rf.flag(HF), rf.flag(CF))
      val carryIn = (op === Alu.ADC || op === Alu.SBC) && cf

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
      switch(op) {
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
      val keepZ = op === Alu.CPL || op === Alu.SCF || op === Alu.CCF
      val z = keepZ ? zf | (calc === 0)
      seq.addSource(Alu.RESULT, calc)
      seq.addSource(Alu.FLAGS, (z ## n ## h ## c ## B"0000").asUInt)

      val hl = rf.read(HL)
      for (rr <- r16) {
        val v = rf.read(rr)
        val s = hl.resize(17) + v.resize(17)
        val h12 = (hl(11 downto 0).resize(13) + v(11 downto 0).resize(13))(12)
        seq.addSource(Alu.ADD_HL(rr), s(15 downto 0))
        seq.addSource(Alu.ADD_HL_FLAGS(rr), (zf ## False ## h12 ## s(16) ## B"0000").asUInt)
      }
      val sp = rf.read(SP)
      val e = rf.read(Z)
      seq.addSource(Alu.SP_E, (sp.asSInt + e.asSInt).asUInt)
      val spH = (sp(3 downto 0).resize(5) + e(3 downto 0).resize(5))(4)
      val spC = (sp(7 downto 0).resize(9) + e.resize(9))(8)
      seq.addSource(Alu.SP_E_FLAGS, (B"00" ## spH ## spC ## B"0000").asUInt)
      retainer.release()
    }
  }

  class ShifterPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      Seq(Shifter.OP, Shifter.SRC, Shifter.BIT).foreach(seq.addKey)
      val op = seq.ctrl(Shifter.OP)

      val v = operand(rf, seq.ctrl(Shifter.SRC))
      val cf = rf.flag(CF)
      val mask = U(1, 8 bits) |<< seq.ctrl(Shifter.BIT)
      val res = UInt(8 bits)
      val carryOut = Bool()
      res := v
      carryOut := False
      switch(op) {
        is(Shifter.RLC) { res := v.rotateLeft(1); carryOut := v(7) }
        is(Shifter.RRC) { res := v.rotateRight(1); carryOut := v(0) }
        is(Shifter.RL) { res := (v(6 downto 0) ## cf).asUInt; carryOut := v(7) }
        is(Shifter.RR) { res := (cf ## v(7 downto 1)).asUInt; carryOut := v(0) }
        is(Shifter.SLA) { res := v |<< 1; carryOut := v(7) }
        is(Shifter.SRA) { res := (v(7) ## v(7 downto 1)).asUInt; carryOut := v(0) }
        is(Shifter.SWAP) { res := (v(3 downto 0) ## v(7 downto 4)).asUInt }
        is(Shifter.SRL) { res := v |>> 1; carryOut := v(0) }
        is(Shifter.RES) { res := v & ~mask }
        is(Shifter.SET) { res := v | mask }
      }
      val isTest = op === Shifter.TEST
      val flags = isTest ? (((v & mask) === 0) ## False ## True ## cf) | ((res === 0) ## False ## False ## carryOut)
      seq.addSource(Shifter.RESULT, res)
      seq.addSource(Shifter.FLAGS, (flags ## B"0000").asUInt)
      seq.addSource(Shifter.FLAGS_Z0, (False ## flags(2 downto 0) ## B"0000").asUInt)
      retainer.release()
    }
  }

  /** The SoC side of the interrupt interface, implemented by the plugin holding IE and IF
    * (`retro.gb.InterruptControllerPlugin`). Both signals must exist from setup on. */
  trait InterruptController {
    /** IE & IF, bit 0 VBlank ... bit 4 Joypad. */
    def cpuRequests: Bits
    /** Pulses the index being serviced, so the controller clears that IF bit. */
    def cpuAck: Flow[UInt]
  }

  /** IME, EI/DI/RETI and interrupt dispatch. `requests` is IE & IF (bit 0 VBlank ... bit 4
    * Joypad); `ack` pulses the index being serviced so its IF bit can be cleared. When the host
    * has an `InterruptController` plugin they are wired to it; otherwise they are pins, so the
    * same plugin serves a bare CPU and a whole console.
    *
    * Interrupts are taken at an instruction-ending fetch: the fetched opcode is discarded and
    * IR loads the dispatch opcode (0x200), which pushes PC and jumps to 0x40 + 8 * index.
    * EI takes effect after the following instruction; DI and RETI immediately. */
  class InterruptPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val controller = host.get[InterruptController]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      seq.addKey(Interrupts.IME_OP)
      val imeOp = seq.ctrl(Interrupts.IME_OP)

      val requests = Bits(5 bits)
      val ack = Flow(UInt(3 bits))
      controller match {
        case Some(c) => requests := c.cpuRequests; c.cpuAck := ack
        case None => in(requests); master(ack)
      }
      val ime = RegInit(False).simPublic()
      val imeNext = RegInit(False)
      val vector = Reg(UInt(3 bits)) init 0

      val pending = requests =/= 0
      val enabled = ime || imeNext
      val effective = imeOp === Interrupts.RETI || (enabled && imeOp =/= Interrupts.DI)
      val dispatch = effective && pending
      val index = OHToUInt(OHMasking.first(requests))

      seq.addFetchOverride(dispatch, U(DISPATCH, 10 bits))
      when(seq.fetchFire) {
        ime := effective && !dispatch
        imeNext := imeOp === Interrupts.EI && !dispatch
        when(dispatch) { vector := index }
      }
      ack.valid := seq.fetchFire && dispatch
      ack.payload := index

      seq.addSource(Interrupts.VECTOR, U(0x40, 16 bits) + (vector @@ U"000").resized)
      seq.addCond(Interrupts.PENDING, pending)
      seq.addCond(Interrupts.NOT_PENDING, !pending)
      seq.addCond(Interrupts.HALT_BUG, pending && !enabled)
      retainer.release()
    }
  }

  /** The opcode table, plus the IDU and the small values in `Isa`. */
  class IsaPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()

      for (p <- Idu.pairs) {
        seq.addSource(Idu.inc(p), rf.read(p) + 1)
        seq.addSource(Idu.dec(p), rf.read(p) - 1)
      }
      val pc = rf.read(PC)
      val z = rf.read(Z)
      seq.addSource(Isa.PC_E, (pc.asSInt + z.asSInt).asUInt)
      seq.addSource(Isa.PC_HI, pc(15 downto 8))
      seq.addSource(Isa.PC_LO, pc(7 downto 0))
      for ((v, n) <- Isa.RST.zipWithIndex) seq.addSource(v, U(n * 8, 16 bits))
      seq.addSource(Isa.Z_F0, z & 0xF0)
      seq.addSource(Isa.HIGH_Z, U(0xFF, 8 bits) @@ z)
      seq.addSource(Isa.HIGH_C, U(0xFF, 8 bits) @@ rf.read(C))
      opcodes.foreach(seq.addOp)
      retainer.release()
    }
  }

  import MCycleSpec.{idle, read, write}
  import SequencerPlugin.RDATA
  val fetch = MCycleSpec.fetch(PC).assign(PC -> Idu.inc(PC))
  val imm = read(PC).assign(PC -> Idu.inc(PC))
  /** What unused opcodes do: fetch the next one, as a NOP. */
  val illegal = fetch

  lazy val opcodes: Seq[OpSpec] = {
    def alu(c: MCycleSpec, op: Int, src: Int = 7) = c.set(Alu.OP, op).set(Alu.SRC, src)
    def shift(c: MCycleSpec, op: Int, src: Int, bit: Int = 0) = c.set(Shifter.OP, op).set(Shifter.SRC, src).set(Shifter.BIT, bit)
    // Push in the SM83 order: SP-1, write the high byte, SP-1, write the low byte.
    def push(hi: Source, lo: Source, last: MCycleSpec => MCycleSpec = identity) = Seq(
      idle.assign(SP -> Idu.dec(SP)), write(SP, hi).assign(SP -> Idu.dec(SP)), last(write(SP, lo)))
    def pop(lo: Sink, hi: Sink) = Seq(
      read(SP).assign(lo -> RDATA, SP -> Idu.inc(SP)), read(SP).assign(hi -> RDATA, SP -> Idu.inc(SP)))
    val pushPC = (last: MCycleSpec => MCycleSpec) => push(Isa.PC_HI, Isa.PC_LO, last)
    val ops = Seq.newBuilder[OpSpec]
    def op(name: String, opcode: Int, cycles: MCycleSpec*) = ops += OpSpec(name, opcode, cycles)

    // ---- 8-bit loads ---------------------------------------------------------------------
    op("NOP", 0x00, fetch)
    for (d <- 0 to 7; s <- 0 to 7 if !(d == 6 && s == 6)) {
      val name = s"LD ${r8Names(d)},${r8Names(s)}"
      val code = 0x40 | d << 3 | s
      if (s == 6) op(name, code, read(HL).assign(r8(d) -> RDATA), fetch)
      else if (d == 6) op(name, code, write(HL, r8(s)), fetch)
      else op(name, code, fetch.assign(r8(d) -> r8(s)))
    }
    for (d <- 0 to 7) {
      val code = 0x06 | d << 3
      if (d == 6) op("LD (HL),n", code, imm.assign(Z -> RDATA), write(HL, Z), fetch)
      else op(s"LD ${r8Names(d)},n", code, imm.assign(r8(d) -> RDATA), fetch)
    }
    op("LD (BC),A", 0x02, write(BC, A), fetch)
    op("LD (DE),A", 0x12, write(DE, A), fetch)
    op("LD A,(BC)", 0x0A, read(BC).assign(A -> RDATA), fetch)
    op("LD A,(DE)", 0x1A, read(DE).assign(A -> RDATA), fetch)
    op("LD (HL+),A", 0x22, write(HL, A).assign(HL -> Idu.inc(HL)), fetch)
    op("LD (HL-),A", 0x32, write(HL, A).assign(HL -> Idu.dec(HL)), fetch)
    op("LD A,(HL+)", 0x2A, read(HL).assign(A -> RDATA, HL -> Idu.inc(HL)), fetch)
    op("LD A,(HL-)", 0x3A, read(HL).assign(A -> RDATA, HL -> Idu.dec(HL)), fetch)
    op("LDH (n),A", 0xE0, imm.assign(Z -> RDATA), write(Isa.HIGH_Z, A), fetch)
    op("LDH A,(n)", 0xF0, imm.assign(Z -> RDATA), read(Isa.HIGH_Z).assign(A -> RDATA), fetch)
    op("LD (C),A", 0xE2, write(Isa.HIGH_C, A), fetch)
    op("LD A,(C)", 0xF2, read(Isa.HIGH_C).assign(A -> RDATA), fetch)
    op("LD (nn),A", 0xEA, imm.assign(Z -> RDATA), imm.assign(W -> RDATA), write(WZ, A), fetch)
    op("LD A,(nn)", 0xFA, imm.assign(Z -> RDATA), imm.assign(W -> RDATA), read(WZ).assign(A -> RDATA), fetch)

    // ---- 16-bit loads, arithmetic and stack ------------------------------------------------
    for ((rr, i) <- r16.zipWithIndex) {
      op(s"LD $rr,nn", 0x01 | i << 4, imm.assign(rr.lo -> RDATA), imm.assign(rr.hi -> RDATA), fetch)
      op(s"INC $rr", 0x03 | i << 4, idle.assign(rr -> Idu.inc(rr)), fetch)
      op(s"DEC $rr", 0x0B | i << 4, idle.assign(rr -> Idu.dec(rr)), fetch)
      op(s"ADD HL,$rr", 0x09 | i << 4, idle.assign(HL -> Alu.ADD_HL(rr), F -> Alu.ADD_HL_FLAGS(rr)), fetch)
    }
    op("LD (nn),SP", 0x08, imm.assign(Z -> RDATA), imm.assign(W -> RDATA),
      write(WZ, SPL).assign(WZ -> Idu.inc(WZ)), write(WZ, SPH), fetch)
    op("LD SP,HL", 0xF9, idle.assign(SP -> HL), fetch)
    op("ADD SP,e", 0xE8, imm.assign(Z -> RDATA), idle.assign(SP -> Alu.SP_E, F -> Alu.SP_E_FLAGS), idle, fetch)
    op("LD HL,SP+e", 0xF8, imm.assign(Z -> RDATA), idle.assign(HL -> Alu.SP_E, F -> Alu.SP_E_FLAGS), fetch)
    for ((rr, i) <- r16Stack.zipWithIndex) {
      op(s"PUSH $rr", 0xC5 | i << 4, push(rr.hi, rr.lo) :+ fetch: _*)
      if (rr == Sm83Regs.AF) op("POP AF", 0xF1, read(SP).assign(Z -> RDATA, SP -> Idu.inc(SP)),
        read(SP).assign(A -> RDATA, F -> Isa.Z_F0, SP -> Idu.inc(SP)), fetch)
      else op(s"POP $rr", 0xC1 | i << 4, pop(rr.lo, rr.hi) :+ fetch: _*)
    }

    // ---- 8-bit ALU -------------------------------------------------------------------------
    for (r <- 0 to 7; (name, aop, code) <- Seq(("INC", Alu.INC, 0x04 | r << 3), ("DEC", Alu.DEC, 0x05 | r << 3))) {
      if (r == 6) op(s"$name (HL)", code, read(HL).assign(Z -> RDATA),
        alu(write(HL, Alu.RESULT).assign(F -> Alu.FLAGS), aop, 6), fetch)
      else op(s"$name ${r8Names(r)}", code, alu(fetch.assign(r8(r) -> Alu.RESULT, F -> Alu.FLAGS), aop, r))
    }
    for (o <- 0 to 7) {
      // CP only updates the flags.
      def writeBack(c: MCycleSpec) = if (o == Alu.CP) c.assign(F -> Alu.FLAGS) else c.assign(A -> Alu.RESULT, F -> Alu.FLAGS)
      for (r <- 0 to 7) {
        val code = 0x80 | o << 3 | r
        val name = s"${Alu.names(o)} A,${r8Names(r)}"
        if (r == 6) op(name, code, read(HL).assign(Z -> RDATA), alu(writeBack(fetch), o, 6))
        else op(name, code, alu(writeBack(fetch), o, r))
      }
      op(s"${Alu.names(o)} A,n", 0xC6 | o << 3, imm.assign(Z -> RDATA), alu(writeBack(fetch), o, 6))
    }
    for ((name, aop, code) <- Seq(("DAA", Alu.DAA, 0x27), ("CPL", Alu.CPL, 0x2F), ("SCF", Alu.SCF, 0x37), ("CCF", Alu.CCF, 0x3F)))
      op(name, code, alu(fetch.assign(A -> Alu.RESULT, F -> Alu.FLAGS), aop))
    for ((name, sop, code) <- Seq(("RLCA", Shifter.RLC, 0x07), ("RRCA", Shifter.RRC, 0x0F), ("RLA", Shifter.RL, 0x17), ("RRA", Shifter.RR, 0x1F)))
      op(name, code, shift(fetch.assign(A -> Shifter.RESULT, F -> Shifter.FLAGS_Z0), sop, 7))

    // ---- Control flow ----------------------------------------------------------------------
    op("JR e", 0x18, imm.assign(Z -> RDATA), idle.assign(PC -> Isa.PC_E), fetch)
    op("JP nn", 0xC3, imm.assign(Z -> RDATA), imm.assign(W -> RDATA), idle.assign(PC -> WZ), fetch)
    op("JP HL", 0xE9, MCycleSpec.fetch(HL).assign(PC -> Idu.inc(HL)))
    op("CALL nn", 0xCD, imm.assign(Z -> RDATA) +: imm.assign(W -> RDATA) +: pushPC(_.assign(PC -> WZ)) :+ fetch: _*)
    op("RET", 0xC9, pop(Z, W) :+ idle.assign(PC -> WZ) :+ fetch: _*)
    op("RETI", 0xD9, pop(Z, W) :+ idle.assign(PC -> WZ) :+ fetch.set(Interrupts.IME_OP, Interrupts.RETI): _*)
    for ((rst, n) <- Isa.RST.zipWithIndex) op(rst.name.patch(3, " ", 0), 0xC7 | n << 3,
      pushPC(_.assign(PC -> rst)) :+ fetch: _*)
    // Conditional forms branch to their final fetch when the condition fails.
    for (((cc, fail), i) <- conditions.zipWithIndex) {
      op(s"JR $cc,e", 0x20 | i << 3, imm.assign(Z -> RDATA).branchIf(fail, 2), idle.assign(PC -> Isa.PC_E), fetch)
      op(s"JP $cc,nn", 0xC2 | i << 3, imm.assign(Z -> RDATA), imm.assign(W -> RDATA).branchIf(fail, 3), idle.assign(PC -> WZ), fetch)
      op(s"CALL $cc,nn", 0xC4 | i << 3, imm.assign(Z -> RDATA) +: imm.assign(W -> RDATA).branchIf(fail, 5) +:
        pushPC(_.assign(PC -> WZ)) :+ fetch: _*)
      op(s"RET $cc", 0xC0 | i << 3, idle.branchIf(fail, 4) +: pop(Z, W) :+ idle.assign(PC -> WZ) :+ fetch: _*)
    }

    // ---- Interrupt control ------------------------------------------------------------------
    op("DI", 0xF3, fetch.set(Interrupts.IME_OP, Interrupts.DI))
    op("EI", 0xFB, fetch.set(Interrupts.IME_OP, Interrupts.EI))
    // HALT waits for a pending interrupt. Entered with one already pending and IME off, the
    // next fetch does not increment PC (the HALT bug).
    op("HALT", 0x76, idle.branchIf(Interrupts.HALT_BUG, 3), idle.branchIf(Interrupts.NOT_PENDING, 1), fetch, MCycleSpec.fetch(PC))
    op("INT", DISPATCH, idle.assign(PC -> Idu.dec(PC)) +: pushPC(_.assign(PC -> Interrupts.VECTOR)) :+ fetch: _*)

    // ---- CB page -----------------------------------------------------------------------------
    op("PREFIX CB", 0xCB, MCycleSpec.prefix(PC, 1).assign(PC -> Idu.inc(PC)))
    for (x <- 0 to 255) {
      val (r, y, group) = (x & 7, (x >> 3) & 7, x >> 6)
      val code = CB_PAGE | x
      val (name, sop, writes, flags) = group match {
        case 0 => (s"${Shifter.names(y)} ${r8Names(r)}", y, true, true)
        case 1 => (s"BIT $y,${r8Names(r)}", Shifter.TEST, false, true)
        case 2 => (s"RES $y,${r8Names(r)}", Shifter.RES, true, false)
        case 3 => (s"SET $y,${r8Names(r)}", Shifter.SET, true, false)
      }
      def result(c: MCycleSpec) = if (flags) c.assign(F -> Shifter.FLAGS) else c
      if (r == 6) {
        val load = read(HL).assign(Z -> RDATA)
        if (writes) op(name, code, load, shift(result(write(HL, Shifter.RESULT)), sop, 6, y), fetch)
        else op(name, code, load, shift(result(fetch), sop, 6, y))
      } else op(name, code, shift(result(if (writes) fetch.assign(r8(r) -> Shifter.RESULT) else fetch), sop, r, y))
    }
    ops.result()
  }

  /** Mnemonics by opcode (CB xx as 0x1xx), for traces. */
  lazy val names: Map[Int, String] = opcodes.map(o => o.opcode -> o.name).toMap

  def plugins(withTrace: Boolean = true): Seq[Hostable] = Seq(
    new BusPlugin(RetroBusConfig(16, 8)),
    new SequencerPlugin(opcodeWidth = 10, illegal = illegal),
    new RegFilePlugin(Sm83Regs.spec),
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
