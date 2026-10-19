package retro.r2a03

import retro.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin._

/** 2A03 registers, pairs and flags. ADH/ADL hold the effective address being built and D the
  * data latch: every read operand goes through D, and read-modify-write ops modify D in place.
  * P keeps bit 5 at 1 and bit 4 (B) at 0; B only exists on the stack. */
object R2A03Regs {
  val A = Register("A", 8); val X = Register("X", 8); val Y = Register("Y", 8)
  val SP = Register("SP", 8); val P = Register("P", 8, init = 0x20)
  val PCH = Register("PCH", 8); val PCL = Register("PCL", 8)
  val ADH = Register("ADH", 8); val ADL = Register("ADL", 8)
  val D = Register("D", 8)

  val PC = Pair("PC", PCH, PCL); val AD = Pair("AD", ADH, ADL)

  val CF = Flag("CF", P, 0); val ZF = Flag("ZF", P, 1); val IF = Flag("IF", P, 2)
  val DF = Flag("DF", P, 3); val VF = Flag("VF", P, 6); val NF = Flag("NF", P, 7)

  val spec = RegFileSpec(Seq(A, X, Y, SP, P, PCH, PCL, ADH, ADL, D), Seq(PC, AD), Seq(CF, ZF, IF, DF, VF, NF))
  val architectural = Seq(A, X, Y, SP, P, PCH, PCL)
}
import R2A03Regs._

/** Address generation: increments, zero page, stack page and indexing, with the carry out of
  * the low byte that decides whether an indexed access costs a fix-up cycle. */
object Agu {
  val PC_INC = Value("PC+1")
  val PC_DEC = Value("PC-1")
  val STACK = Value("0100+SP")
  val S_INC = Value("SP+1")
  val S_DEC = Value("SP-1")
  val ZP = Value("00:ADL")
  val ADL_INC = Value("ADL+1")
  val ADH_INC = Value("ADH+1")
  val AD_INC = Value("AD+1")
  /** ADH with ADL+1, without carrying into ADH: the JMP (ind) page wrap. */
  val AD_LO_INC = Value("ADH:ADL+1")
  /** PC plus the signed branch offset in D. */
  val PC_REL = Value("PC+D")
  val BRANCH_SAME_PAGE = Cond("PC+D same page")

  /** Indexing by X or Y: the low byte alone, the address before the carry is fixed up (where the
    * dummy read goes), the full sum, and whether the low byte carried. */
  class Index(val reg: Register) {
    val lo = Value(s"ADL+$reg")
    val uncarried = Value(s"ADH:ADL+$reg")
    val full = Value(s"AD+$reg")
    val noCarry = Cond(s"ADL+$reg no carry")
  }
  val byX = new Index(X)
  val byY = new Index(Y)
}

/** The ALU: one operation per cycle on a selected left operand and D. */
object Alu {
  val OP = CtrlKey("ALU_OP", 5)
  val SRC = CtrlKey("ALU_SRC", 3)
  val RESULT = Value("alu")
  val N = Value("alu.N"); val Z = Value("alu.Z"); val C = Value("alu.C"); val V = Value("alu.V")

  // Left operand: A, X, Y, SP, D, or A & X (SAX, AXS, SHA, TAS)
  val SRC_A = 0; val SRC_X = 1; val SRC_Y = 2; val SRC_S = 3; val SRC_D = 4; val SRC_AX = 5
  val sources = Map[Register, Int](A -> SRC_A, X -> SRC_X, Y -> SRC_Y, SP -> SRC_S, D -> SRC_D)

  val PASS = 0; val ORA = 1; val AND = 2; val EOR = 3; val ADC = 4; val SBC = 5; val CMP = 6; val BIT = 7
  val INC = 8; val DEC = 9; val ASL = 10; val LSR = 11; val ROL = 12; val ROR = 13
  val ANC = 14; val ALR = 15; val ARR = 16; val ANE = 17; val LXA = 18
  /** The constant ORed into A by the unstable ANE and LXA (the value SingleStepTests use). */
  val MAGIC = 0xEE
}

/** NMI, IRQ, BRK and reset. */
object Interrupts {
  /** Set on the cycle that pushes P in BRK and in the dispatch: the vector is chosen then
    * (an NMI arriving before it hijacks a BRK or an IRQ) and a taken NMI is acknowledged. */
  val SELECT = CtrlKey("INT_SELECT", 1)
  /** Set on a branch's operand read, which does not poll. A taken branch that stays on its page
    * therefore acts on the poll of its opcode fetch, one cycle earlier than the usual rule. */
  val HOLD_POLL = CtrlKey("INT_HOLD_POLL", 1)
  /** Discards the poll: the interrupt sequence's vector reads, so a handler's first instruction
    * always runs. */
  val CLEAR_POLL = CtrlKey("INT_CLEAR_POLL", 1)
  val VECTOR_LO = Value("vector.lo")
  val VECTOR_HI = Value("vector.hi")
}

/** Constants and small values that belong to no other unit. */
object Isa {
  /** P as BRK and PHP push it (B set), and as IRQ and NMI push it (B clear). */
  val P_BRK = Value("P|30")
  val P_IRQ = Value("P&EF")
  /** The byte being read, as P: what PLP and RTI pull (bit 5 forced to 1, B dropped). */
  val RDATA_AS_P = Value("rdata->P")
  val A_AND_X = Value("A&X")
  val ZERO = Value("0")
  val ONE = Value("1")
  val ALWAYS = Cond("always")
  val RESET_LO = Value("FFFC"); val RESET_HI = Value("FFFD")
  val JAM_FFFF = Value("FFFF"); val JAM_FFFE = Value("FFFE")
  /** The unstable SHA/SHX/SHY/TAS stores: the data is `reg & D`, where D holds the base
    * address high byte + 1; when indexing carried, that value also replaces the high byte of
    * the address. */
  class Sh(val name: String) { val data = Value(s"$name.data"); val address = Value(s"$name.addr") }
  val SH_A_X = new Sh("SH(A&X)"); val SH_X = new Sh("SH(X)"); val SH_Y = new Sh("SH(Y)")
}

/** Ricoh 2A03 (NES CPU): a 6502 without decimal mode, described as data on top of `retro.core`.
  *
  * Every one of the 256 opcodes is implemented, the undocumented ones included, with each cycle's
  * bus access (dummy reads and writes too) as on the real chip. The D flag can be set and
  * cleared but ADC/SBC stay binary. One clock is one CPU cycle; every cycle is a read or a write.
  *
  * Opcode space is 9 bits: 0x000-0x0FF the opcodes, 0x100 the IRQ/NMI dispatch sequence and
  * 0x101 the reset sequence, which is what IR holds at reset.
  *
  * The 6502 executes the last step of most instructions while it fetches the next opcode. The
  * spec says the same thing: a read operand lands in D, and the instruction's final `fetch`
  * cycle does the ALU operation into A, X or Y. */
object Ricoh2A03 {
  val DISPATCH = 0x100
  val RESET = 0x101

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
      val d = rf.read(D)
      val l = Vec(Seq(a, rf.read(X), rf.read(Y), rf.read(SP), d, a & rf.read(X), d, d))(seq.ctrl(Alu.SRC))
      val cin = rf.flag(CF)
      // Adder shared by ADC, SBC and CMP: SBC and CMP add the complement.
      val subtract = op === Alu.SBC || op === Alu.CMP
      val rhs = subtract ? ~d | d
      val carry = (op === Alu.CMP) || ((op === Alu.ADC || op === Alu.SBC) && cin)
      val sum = l.resize(9) + rhs.resize(9) + carry.asUInt.resize(9)
      val overflow = (~(l ^ rhs) & (l ^ sum(7 downto 0)))(7)
      val ld = l & d

      val r = UInt(8 bits)
      val c, v = Bool()
      r := l; c := cin; v := rf.flag(VF)
      switch(op) {
        is(Alu.ORA) { r := l | d }
        is(Alu.AND) { r := ld }
        is(Alu.EOR) { r := l ^ d }
        is(Alu.ADC, Alu.SBC) { r := sum(7 downto 0); c := sum(8); v := overflow }
        is(Alu.CMP) { r := sum(7 downto 0); c := sum(8) }
        is(Alu.BIT) { v := d(6) }
        is(Alu.INC) { r := l + 1 }
        is(Alu.DEC) { r := l - 1 }
        is(Alu.ASL) { r := l |<< 1; c := l(7) }
        is(Alu.LSR) { r := l |>> 1; c := l(0) }
        is(Alu.ROL) { r := (l(6 downto 0) ## cin).asUInt; c := l(7) }
        is(Alu.ROR) { r := (cin ## l(7 downto 1)).asUInt; c := l(0) }
        is(Alu.ANC) { r := ld; c := ld(7) }
        is(Alu.ALR) { r := ld |>> 1; c := ld(0) }
        is(Alu.ARR) {
          val t = (cin ## ld(7 downto 1)).asUInt
          r := t; c := t(6); v := t(6) ^ t(5)
        }
        is(Alu.ANE) { r := (a | Alu.MAGIC) & rf.read(X) & d }
        is(Alu.LXA) { r := (a | Alu.MAGIC) & d }
      }
      val isBit = op === Alu.BIT
      seq.addSource(Alu.RESULT, r)
      seq.addSource(Alu.N, (isBit ? d(7) | r(7)).asUInt)
      seq.addSource(Alu.Z, (isBit ? (ld === 0) | (r === 0)).asUInt)
      seq.addSource(Alu.C, c.asUInt)
      seq.addSource(Alu.V, v.asUInt)
      retainer.release()
    }
  }

  /** The SoC side of the interrupt lines (a console's APU frame counter, mapper or PPU drive
    * them). Both must exist from setup on. */
  trait InterruptLines {
    /** The NMI request, active high (the inverse of the /NMI pin); taken on its rising edge. */
    def nmi: Bool
    /** The IRQ request, active high and level-triggered. */
    def irq: Bool
    /** A soft reset request, or null: while high, the next opcode fetch enters the reset
      * sequence instead, and the interrupt state is cleared. */
    def reset: Bool = null
  }

  /** NMI edge detection, IRQ masking and dispatch. With an `InterruptLines` plugin in the host
    * the lines are wired to it; otherwise they are pins.
    *
    * The 6502 polls interrupts in the second-to-last cycle of an instruction: every completed
    * cycle registers a poll, and a fetch acts on the poll of the cycle before the previous one.
    * CLI, SEI and PLP change I after their poll, so they take effect one instruction late, while
    * RTI pulls P early enough for its own poll to see it. When it holds, the fetched opcode is replaced by
    * the dispatch sequence (0x100), which reads at PC again, pushes PC and P (B clear) and jumps
    * through the vector chosen on the cycle PCL is pushed (an NMI seen by then hijacks BRK or
    * IRQ). The vector reads discard the poll, so the handler's first instruction always runs.
    *
    * Branches do not poll in their operand read (`HOLD_POLL`), so a taken branch that stays on
    * its page acts on the poll of its opcode fetch, and an interrupt arriving later waits one more
    * instruction. */
  class InterruptPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val lines = host.get[InterruptLines]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      seq.addKey(Interrupts.SELECT)
      seq.addKey(Interrupts.HOLD_POLL)
      seq.addKey(Interrupts.CLEAR_POLL)

      val nmi, irq = Bool()
      lines match {
        case Some(l) => nmi := l.nmi; irq := l.irq
        case None => in(nmi, irq); nmi.setName("nmi"); irq.setName("irq")
      }
      val nmiLast = RegInit(False)
      val nmiPending = RegInit(False).simPublic()
      val nmiVector = RegInit(False)
      // `polled` is what the last completed cycle polled; the fetch acts on `decided`, the poll of
      // the cycle before it, which is the instruction's second-to-last cycle.
      val polled = RegInit(False).simPublic()
      val decided = RegInit(False).simPublic()
      val select = seq.cycleDone && seq.ctrl(Interrupts.SELECT) === 1

      // An NMI edge counts in the cycle it is seen: for that cycle's poll and vector choice. While
      // the bus is stalled (a DMA has it) edges are still caught, and nothing is polled.
      val pending = nmiPending || (nmi && !nmiLast)
      nmiLast := nmi
      nmiPending := pending
      when(seq.cycleDone) {
        when(select) { nmiVector := pending; nmiPending := False }
        when(seq.ctrl(Interrupts.CLEAR_POLL) === 1) { polled := False }
          .elsewhen(seq.ctrl(Interrupts.HOLD_POLL) === 0) { polled := pending || (irq && !rf.flag(IF)) }
        decided := polled
      }

      seq.addFetchOverride(decided, U(DISPATCH, 9 bits))
      for (r <- lines.flatMap(l => Option(l.reset))) {
        when(seq.fetchFire && r) { nmiLast := False; nmiPending := False; nmiVector := False; polled := False; decided := False }
        seq.addFetchOverride(r, U(RESET, 9 bits))
      }
      // Vectors are picked by the high byte so BRK and dispatch share the table.
      seq.addSource(Interrupts.VECTOR_LO, nmiVector ? U(0xFFFA, 16 bits) | U(0xFFFE, 16 bits))
      seq.addSource(Interrupts.VECTOR_HI, nmiVector ? U(0xFFFB, 16 bits) | U(0xFFFF, 16 bits))
      retainer.release()
    }
  }

  /** Address generation (`Agu`), the small values in `Isa` and the opcode table. */
  class IsaPlugin extends FiberPlugin {
    val logic = during setup new Area {
      val seq = host[SequencerService]
      val rf = host[RegFileService]
      val retainer = retains(seq.seqLock)
      awaitBuild()
      val pc = rf.read(PC)
      val ad = rf.read(AD)
      val (adh, adl, d, s) = (rf.read(ADH), rf.read(ADL), rf.read(D), rf.read(SP))
      seq.addSource(Agu.PC_INC, pc + 1)
      seq.addSource(Agu.PC_DEC, pc - 1)
      seq.addSource(Agu.STACK, U(0x01, 8 bits) @@ s)
      seq.addSource(Agu.S_INC, s + 1)
      seq.addSource(Agu.S_DEC, s - 1)
      seq.addSource(Agu.ZP, adl.resize(16))
      seq.addSource(Agu.ADL_INC, adl + 1)
      seq.addSource(Agu.ADH_INC, adh + 1)
      seq.addSource(Agu.AD_INC, ad + 1)
      seq.addSource(Agu.AD_LO_INC, adh @@ (adl + 1))
      val target = (pc.asSInt + d.asSInt.resize(16)).asUInt
      seq.addSource(Agu.PC_REL, target)
      seq.addCond(Agu.BRANCH_SAME_PAGE, target(15 downto 8) === pc(15 downto 8))
      for (ix <- Seq(Agu.byX, Agu.byY)) {
        val i = rf.read(ix.reg)
        val lo = adl.resize(9) + i.resize(9)
        seq.addSource(ix.lo, lo(7 downto 0))
        seq.addSource(ix.uncarried, adh @@ lo(7 downto 0))
        seq.addSource(ix.full, ad + i.resize(16))
        seq.addCond(ix.noCarry, !lo(8))
      }

      val p = rf.read(P)
      val ax = rf.read(A) & rf.read(X)
      seq.addSource(Isa.P_BRK, p | 0x30)
      seq.addSource(Isa.P_IRQ, (p & 0xEF) | 0x20)
      seq.addSource(Isa.RDATA_AS_P, (seq.rdata & 0xCF) | 0x20, fromRdata = true)
      seq.addSource(Isa.A_AND_X, ax)
      seq.addSource(Isa.ZERO, U(0, 1 bits))
      seq.addSource(Isa.ONE, U(1, 1 bits))
      seq.addCond(Isa.ALWAYS, True)
      for ((v, a) <- Seq(Isa.RESET_LO -> 0xFFFC, Isa.RESET_HI -> 0xFFFD, Isa.JAM_FFFF -> 0xFFFF, Isa.JAM_FFFE -> 0xFFFE))
        seq.addSource(v, U(a, 16 bits))
      for ((sh, reg) <- Seq(Isa.SH_A_X -> ax, Isa.SH_X -> rf.read(X), Isa.SH_Y -> rf.read(Y))) {
        val data = reg & d
        seq.addSource(sh.data, data)
        seq.addSource(sh.address, (adh === d) ? (data @@ adl) | ad)
      }
      opcodes.foreach(seq.addOp)
      retainer.release()
    }
  }

  import MCycleSpec.{read, write}
  import SequencerPlugin.RDATA
  val fetch = MCycleSpec.fetch(PC).assign(PC -> Agu.PC_INC)
  /** Read the byte at PC and step past it. */
  def imm(to: Sink) = read(PC).assign(to -> RDATA, PC -> Agu.PC_INC)
  /** The read at PC that one-byte instructions make while they work out what to do. */
  val dummy = read(PC)
  val illegal = fetch

  /** An ALU operation in a cycle: what it computes and which flags it updates. */
  case class AluOp(op: Int, src: Int, flags: Seq[Flag]) {
    def apply(c: MCycleSpec, to: Sink*): MCycleSpec = {
      val flagSrc = Map[Flag, Value](NF -> Alu.N, ZF -> Alu.Z, CF -> Alu.C, VF -> Alu.V)
      c.set(Alu.OP, op).set(Alu.SRC, src).assign(to.map(_ -> Alu.RESULT) ++ flags.map(f => f -> flagSrc(f)): _*)
    }
  }
  private val NZ = Seq(NF, ZF); private val NZC = Seq(NF, ZF, CF); private val NZCV = Seq(NF, ZF, CF, VF)

  /** How an instruction uses its operand. `Read` finishes in the final fetch, `Write` stores,
    * `Modify` is a read-modify-write: read into D, write D back while the ALU modifies it, write
    * the result, then optionally a second operation in the final fetch (SLO, RRA, DCP...). */
  sealed trait Kind
  case class Read(last: MCycleSpec => MCycleSpec) extends Kind
  case class Write(data: Source, also: MCycleSpec => MCycleSpec = identity) extends Kind
  case class Modify(op: AluOp, last: MCycleSpec => MCycleSpec = identity) extends Kind

  /** The cycles from the effective address on. */
  def access(kind: Kind, address: Source): Seq[MCycleSpec] = kind match {
    case Read(last) => Seq(read(address).assign(D -> RDATA), last(fetch))
    case Write(data, also) => Seq(also(write(address, data)), fetch)
    case Modify(op, last) => Seq(read(address).assign(D -> RDATA), op(write(address, D), D), write(address, D), last(fetch))
  }

  sealed trait Mode
  case object Imm extends Mode; case object Zp extends Mode; case object Abs extends Mode
  case object IndX extends Mode; case object IndY extends Mode
  case class ZpIdx(ix: Agu.Index) extends Mode; case class AbsIdx(ix: Agu.Index) extends Mode
  val ZpX = ZpIdx(Agu.byX); val ZpY = ZpIdx(Agu.byY); val AbsX = AbsIdx(Agu.byX); val AbsY = AbsIdx(Agu.byY)
  val modeNames = Map[Mode, String](Imm -> "#n", Zp -> "zp", Abs -> "abs", IndX -> "(zp,X)", IndY -> "(zp),Y",
    ZpX -> "zp,X", ZpY -> "zp,Y", AbsX -> "abs,X", AbsY -> "abs,Y")

  /** The cycles after the opcode fetch for an addressing mode and an operand use. Indexed reads
    * skip the fix-up cycle when the low byte did not carry; writes and read-modify-writes never
    * do, and their extra cycle is a dummy read at the uncarried address. */
  def addressed(mode: Mode, kind: Kind): Seq[MCycleSpec] = {
    def indexed(ix: Agu.Index, base: Seq[MCycleSpec]): Seq[MCycleSpec] = kind match {
      case Read(_) =>
        val skip = base.size + 2
        base ++ (read(ix.uncarried).assign(D -> RDATA, AD -> ix.full).branchIf(ix.noCarry, skip) +: access(kind, AD))
      case _ => base ++ (read(ix.uncarried).assign(AD -> ix.full) +: access(kind, AD))
    }
    // (zp) pointer: low byte into D, high byte into ADH while D moves to ADL.
    val pointer = Seq(read(Agu.ZP).assign(D -> RDATA, ADL -> Agu.ADL_INC), read(Agu.ZP).assign(ADH -> RDATA, ADL -> D))
    mode match {
      case Imm => kind match {
        case Read(last) => Seq(imm(D), last(fetch))
        case _ => sys.error("immediate operands are read only")
      }
      case Zp => imm(ADL) +: access(kind, Agu.ZP)
      case ZpIdx(ix) => Seq(imm(ADL), read(Agu.ZP).assign(ADL -> ix.lo)) ++ access(kind, Agu.ZP)
      case Abs => Seq(imm(ADL), imm(ADH)) ++ access(kind, AD)
      case AbsIdx(ix) => indexed(ix, Seq(imm(ADL), imm(ADH)))
      case IndX => Seq(imm(ADL), read(Agu.ZP).assign(ADL -> Agu.byX.lo)) ++ pointer ++ access(kind, AD)
      case IndY => indexed(Agu.byY, imm(ADL) +: pointer)
    }
  }

  lazy val opcodes: Seq[OpSpec] = {
    val ops = Seq.newBuilder[OpSpec]
    def op(name: String, opcode: Int, cycles: Seq[MCycleSpec]) = ops += OpSpec(name, opcode, cycles)
    def alu(op: Int, src: Register, flags: Seq[Flag]) = AluOp(op, Alu.sources(src), flags)
    def load(to: Register*) = Read(c => alu(Alu.PASS, D, NZ)(c, to: _*))
    def into(o: AluOp, to: Register*) = Read(c => o(c, to: _*))

    // ---- Groups with the regular aaabbbcc layout -----------------------------------------------
    // cc = 01: ORA AND EOR ADC STA LDA CMP SBC; bbb = (zp,X) zp #n abs (zp),Y zp,X abs,Y abs,X
    val mode01 = IndexedSeq(IndX, Zp, Imm, Abs, IndY, ZpX, AbsY, AbsX)
    val group01 = IndexedSeq[(String, Kind)](
      "ORA" -> into(alu(Alu.ORA, A, NZ), A), "AND" -> into(alu(Alu.AND, A, NZ), A), "EOR" -> into(alu(Alu.EOR, A, NZ), A),
      "ADC" -> into(alu(Alu.ADC, A, NZCV), A), "STA" -> Write(A), "LDA" -> load(A),
      "CMP" -> into(alu(Alu.CMP, A, NZC)), "SBC" -> into(alu(Alu.SBC, A, NZCV), A))
    for ((m, b) <- mode01.zipWithIndex; ((name, kind), a) <- group01.zipWithIndex if !(name == "STA" && m == Imm))
      op(s"$name ${modeNames(m)}", a << 5 | b << 2 | 1, addressed(m, kind))

    // cc = 11 (undocumented): the cc = 01 modes with read-modify-write ops, plus SAX/LAX
    val rmwOps = IndexedSeq(("ASL", Alu.ASL), ("ROL", Alu.ROL), ("LSR", Alu.LSR), ("ROR", Alu.ROR), ("DEC", Alu.DEC), ("INC", Alu.INC))
    def shiftC(o: Int) = AluOp(o, Alu.SRC_D, Seq(CF))
    val group11 = IndexedSeq[(String, Kind)](
      "SLO" -> Modify(shiftC(Alu.ASL), c => alu(Alu.ORA, A, NZ)(c, A)),
      "RLA" -> Modify(shiftC(Alu.ROL), c => alu(Alu.AND, A, NZ)(c, A)),
      "SRE" -> Modify(shiftC(Alu.LSR), c => alu(Alu.EOR, A, NZ)(c, A)),
      "RRA" -> Modify(shiftC(Alu.ROR), c => alu(Alu.ADC, A, NZCV)(c, A)),
      "SAX" -> Write(Isa.A_AND_X), "LAX" -> load(A, X),
      "DCP" -> Modify(AluOp(Alu.DEC, Alu.SRC_D, Nil), c => alu(Alu.CMP, A, NZC)(c)),
      "ISC" -> Modify(AluOp(Alu.INC, Alu.SRC_D, Nil), c => alu(Alu.SBC, A, NZCV)(c, A)))
    for ((m0, b) <- mode01.zipWithIndex; ((name, kind), a) <- group11.zipWithIndex if m0 != Imm) {
      val code = a << 5 | b << 2 | 3
      // SAX and LAX index by Y where the others index by X; SAX has no abs,Y form, LAX (zp),Y is normal
      val m = if ((name == "SAX" || name == "LAX") && m0 == ZpX) ZpY else if ((name == "SAX" || name == "LAX") && m0 == AbsX) AbsY else m0
      if (code == 0x93 || code == 0x9B || code == 0x9F || code == 0xBB) {} // SHA, TAS, LAS below
      else op(s"$name ${modeNames(m)}", code, addressed(m, kind))
    }
    // cc = 11, bbb = 010: immediate-only undocumented ops
    op("ANC #n", 0x0B, addressed(Imm, into(alu(Alu.ANC, A, NZC), A)))
    op("ANC #n", 0x2B, addressed(Imm, into(alu(Alu.ANC, A, NZC), A)))
    op("ALR #n", 0x4B, addressed(Imm, into(alu(Alu.ALR, A, NZC), A)))
    op("ARR #n", 0x6B, addressed(Imm, into(alu(Alu.ARR, A, NZCV), A)))
    op("ANE #n", 0x8B, addressed(Imm, into(alu(Alu.ANE, A, NZ), A)))
    op("LXA #n", 0xAB, addressed(Imm, into(alu(Alu.LXA, A, NZ), A, X)))
    op("AXS #n", 0xCB, addressed(Imm, into(AluOp(Alu.CMP, Alu.SRC_AX, NZC), X)))
    op("SBC #n", 0xEB, addressed(Imm, into(alu(Alu.SBC, A, NZCV), A)))
    op("LAS abs,Y", 0xBB, addressed(AbsY, into(alu(Alu.AND, SP, NZ), A, X, SP)))

    // The unstable stores: D keeps ADH+1 of the base address for the data and the address fix.
    def sh(name: String, code: Int, base: Seq[MCycleSpec], ix: Agu.Index, value: Isa.Sh, also: MCycleSpec => MCycleSpec = identity) =
      op(name, code, base ++ Seq(read(ix.uncarried).assign(AD -> ix.full, D -> Agu.ADH_INC), also(write(value.address, value.data)), fetch))
    val abs = Seq(imm(ADL), imm(ADH))
    sh("SHA (zp),Y", 0x93, Seq(imm(ADL), read(Agu.ZP).assign(D -> RDATA, ADL -> Agu.ADL_INC), read(Agu.ZP).assign(ADH -> RDATA, ADL -> D)),
      Agu.byY, Isa.SH_A_X)
    sh("SHA abs,Y", 0x9F, abs, Agu.byY, Isa.SH_A_X)
    sh("TAS abs,Y", 0x9B, abs, Agu.byY, Isa.SH_A_X, _.assign(SP -> Isa.A_AND_X))
    sh("SHY abs,X", 0x9C, abs, Agu.byX, Isa.SH_Y)
    sh("SHX abs,Y", 0x9E, abs, Agu.byY, Isa.SH_X)

    // cc = 10: shifts, INC/DEC, STX/LDX
    val modes10 = Seq(Zp -> 1, Abs -> 3, ZpX -> 5, AbsX -> 7)
    for (((name, o), a) <- rmwOps.take(4).zipWithIndex ++ rmwOps.drop(4).zip(Seq(6, 7)); (m, b) <- modes10) {
      val flags = if (o == Alu.INC || o == Alu.DEC) NZ else NZC
      op(s"$name ${modeNames(m)}", a << 5 | b << 2 | 2, addressed(m, Modify(AluOp(o, Alu.SRC_D, flags))))
    }
    for (((name, o), a) <- rmwOps.take(4).zipWithIndex)
      op(s"$name A", a << 5 | 0x0A, Seq(dummy, alu(o, A, NZC)(fetch, A)))
    for ((m, code) <- Seq(Zp -> 0x86, Abs -> 0x8E, ZpY -> 0x96)) op(s"STX ${modeNames(m)}", code, addressed(m, Write(X)))
    for ((m, code) <- Seq(Imm -> 0xA2, Zp -> 0xA6, Abs -> 0xAE, ZpY -> 0xB6, AbsY -> 0xBE)) op(s"LDX ${modeNames(m)}", code, addressed(m, load(X)))
    // cc = 00: STY/LDY/CPY/CPX/BIT
    for ((m, code) <- Seq(Zp -> 0x84, Abs -> 0x8C, ZpX -> 0x94)) op(s"STY ${modeNames(m)}", code, addressed(m, Write(Y)))
    for ((m, code) <- Seq(Imm -> 0xA0, Zp -> 0xA4, Abs -> 0xAC, ZpX -> 0xB4, AbsX -> 0xBC)) op(s"LDY ${modeNames(m)}", code, addressed(m, load(Y)))
    for ((m, code) <- Seq(Imm -> 0xC0, Zp -> 0xC4, Abs -> 0xCC)) op(s"CPY ${modeNames(m)}", code, addressed(m, into(alu(Alu.CMP, Y, NZC))))
    for ((m, code) <- Seq(Imm -> 0xE0, Zp -> 0xE4, Abs -> 0xEC)) op(s"CPX ${modeNames(m)}", code, addressed(m, into(alu(Alu.CMP, X, NZC))))
    for ((m, code) <- Seq(Zp -> 0x24, Abs -> 0x2C)) op(s"BIT ${modeNames(m)}", code, addressed(m, into(alu(Alu.BIT, A, Seq(NF, ZF, VF)))))

    // ---- Undocumented NOPs (they still make their reads) -------------------------------------
    val nop = Read(identity)
    for (code <- Seq(0x80, 0x82, 0x89, 0xC2, 0xE2)) op("NOP #n", code, addressed(Imm, nop))
    for (code <- Seq(0x04, 0x44, 0x64)) op("NOP zp", code, addressed(Zp, nop))
    for (code <- Seq(0x14, 0x34, 0x54, 0x74, 0xD4, 0xF4)) op("NOP zp,X", code, addressed(ZpX, nop))
    op("NOP abs", 0x0C, addressed(Abs, nop))
    for (code <- Seq(0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC)) op("NOP abs,X", code, addressed(AbsX, nop))
    for (code <- Seq(0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xFA, 0xEA)) op("NOP", code, Seq(dummy, fetch))

    // ---- Implied ----------------------------------------------------------------------------
    def implied(name: String, code: Int, last: MCycleSpec) = op(name, code, Seq(dummy, last))
    for ((name, code, src, dst) <- Seq(("TAX", 0xAA, A, X), ("TAY", 0xA8, A, Y), ("TXA", 0x8A, X, A), ("TYA", 0x98, Y, A), ("TSX", 0xBA, SP, X)))
      implied(name, code, alu(Alu.PASS, src, NZ)(fetch, dst))
    implied("TXS", 0x9A, fetch.assign(SP -> X))
    for ((name, code, o, r) <- Seq(("INX", 0xE8, Alu.INC, X), ("INY", 0xC8, Alu.INC, Y), ("DEX", 0xCA, Alu.DEC, X), ("DEY", 0x88, Alu.DEC, Y)))
      implied(name, code, alu(o, r, NZ)(fetch, r))
    for ((name, code, f, v) <- Seq(("CLC", 0x18, CF, Isa.ZERO), ("SEC", 0x38, CF, Isa.ONE), ("CLV", 0xB8, VF, Isa.ZERO),
                                   ("CLD", 0xD8, DF, Isa.ZERO), ("SED", 0xF8, DF, Isa.ONE)))
      implied(name, code, fetch.assign(f -> v))
    // I changes in the cycle after the poll of CLI and SEI, so the next instruction still runs
    // under the old I and its own poll (in its opcode fetch, for a 2-cycle one) sees the new one.
    op("CLI", 0x58, Seq(dummy.assign(IF -> Isa.ZERO), fetch))
    op("SEI", 0x78, Seq(dummy.assign(IF -> Isa.ONE), fetch))

    // ---- Stack -------------------------------------------------------------------------------
    op("PHA", 0x48, Seq(dummy, write(Agu.STACK, A).assign(SP -> Agu.S_DEC), fetch))
    op("PHP", 0x08, Seq(dummy, write(Agu.STACK, Isa.P_BRK).assign(SP -> Agu.S_DEC), fetch))
    op("PLA", 0x68, Seq(dummy, read(Agu.STACK).assign(SP -> Agu.S_INC), read(Agu.STACK).assign(D -> RDATA), alu(Alu.PASS, D, NZ)(fetch, A)))
    op("PLP", 0x28, Seq(dummy, read(Agu.STACK).assign(SP -> Agu.S_INC), read(Agu.STACK).assign(P -> Isa.RDATA_AS_P), fetch))

    // ---- Control flow ------------------------------------------------------------------------
    // JMP and JSR fetch at the new address straight from AD, so PC only ever holds its successor.
    val toAD = MCycleSpec.fetch(AD).assign(PC -> Agu.AD_INC)
    op("JMP abs", 0x4C, Seq(imm(ADL), read(PC).assign(ADH -> RDATA), toAD))
    op("JMP (abs)", 0x6C, Seq(imm(ADL), imm(ADH), read(AD).assign(D -> RDATA), read(Agu.AD_LO_INC).assign(PCH -> RDATA, PCL -> D), fetch))
    op("JSR abs", 0x20, Seq(imm(ADL), read(Agu.STACK), write(Agu.STACK, PCH).assign(SP -> Agu.S_DEC),
      write(Agu.STACK, PCL).assign(SP -> Agu.S_DEC), read(PC).assign(ADH -> RDATA), toAD))
    op("RTS", 0x60, Seq(dummy, read(Agu.STACK).assign(SP -> Agu.S_INC), read(Agu.STACK).assign(PCL -> RDATA, SP -> Agu.S_INC),
      read(Agu.STACK).assign(PCH -> RDATA), read(PC).assign(PC -> Agu.PC_INC), fetch))
    op("RTI", 0x40, Seq(dummy, read(Agu.STACK).assign(SP -> Agu.S_INC), read(Agu.STACK).assign(P -> Isa.RDATA_AS_P, SP -> Agu.S_INC),
      read(Agu.STACK).assign(PCL -> RDATA, SP -> Agu.S_INC), read(Agu.STACK).assign(PCH -> RDATA), fetch))
    // Branches: operand into D; when taken, a dummy read while PCL moves, and one more while PCH
    // is fixed if the target is on another page.
    for ((name, code, fail) <- Seq(("BPL", 0x10, NF.isSet), ("BMI", 0x30, NF.isClear), ("BVC", 0x50, VF.isSet), ("BVS", 0x70, VF.isClear),
                                   ("BCC", 0x90, CF.isSet), ("BCS", 0xB0, CF.isClear), ("BNE", 0xD0, ZF.isSet), ("BEQ", 0xF0, ZF.isClear)))
      op(s"$name rel", code, Seq(imm(D).set(Interrupts.HOLD_POLL, 1).branchIf(fail, 3), read(PC).assign(PCL -> Agu.PC_REL, AD -> Agu.PC_REL).branchIf(Agu.BRANCH_SAME_PAGE, 3),
        read(PC).assign(PC -> AD), fetch))

    // ---- BRK, interrupts, reset, JAM -----------------------------------------------------------
    def vectored(p: Source, first: MCycleSpec) = Seq(first, write(Agu.STACK, PCH).assign(SP -> Agu.S_DEC),
      write(Agu.STACK, PCL).assign(SP -> Agu.S_DEC).set(Interrupts.SELECT, 1),
      write(Agu.STACK, p).assign(SP -> Agu.S_DEC, IF -> Isa.ONE),
      read(Interrupts.VECTOR_LO).assign(PCL -> RDATA).set(Interrupts.CLEAR_POLL, 1),
      read(Interrupts.VECTOR_HI).assign(PCH -> RDATA).set(Interrupts.CLEAR_POLL, 1), fetch)
    op("BRK", 0x00, vectored(Isa.P_BRK, read(PC).assign(PC -> Agu.PC_INC)))
    // The fetch that took the interrupt already stepped PC: read that byte again and step back.
    op("INT", DISPATCH, vectored(Isa.P_IRQ, read(Agu.PC_DEC).assign(PC -> Agu.PC_DEC)))
    op("RESET", RESET, Seq(dummy, dummy, read(Agu.STACK).assign(SP -> Agu.S_DEC), read(Agu.STACK).assign(SP -> Agu.S_DEC),
      read(Agu.STACK).assign(SP -> Agu.S_DEC, IF -> Isa.ONE), read(Isa.RESET_LO).assign(PCL -> RDATA), read(Isa.RESET_HI).assign(PCH -> RDATA), fetch))
    // JAM halts with the address bus parked on FFFF; only reset leaves (the fetch is never reached).
    for (code <- Seq(0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2))
      op("JAM", code, Seq(dummy, read(Isa.JAM_FFFF), read(Isa.JAM_FFFE), read(Isa.JAM_FFFE), read(Isa.JAM_FFFF).branchIf(Isa.ALWAYS, 4), fetch))
    ops.result()
  }

  /** Mnemonics by opcode, for traces. */
  lazy val names: Map[Int, String] = opcodes.map(o => o.opcode -> o.name).toMap

  /** `external = false` when the host's slaves claim every address (see `BusPlugin`);
    * `romDecoder` as in `SequencerPlugin`. */
  def plugins(withTrace: Boolean = true, external: Boolean = true, romDecoder: Boolean = false): Seq[Hostable] = Seq(
    new BusPlugin(RetroBusConfig(16, 8), external = external),
    new SequencerPlugin(opcodeWidth = 9, resetOpcode = RESET, illegal = illegal, romDecoder = romDecoder),
    new RegFilePlugin(R2A03Regs.spec),
    new AluPlugin,
    new InterruptPlugin,
    new IsaPlugin
  ) ++ (if (withTrace) Seq(new TracePlugin) else Nil)
}

object R2A03Core {
  def apply(withTrace: Boolean = true): RetroCore = RetroCore(Ricoh2A03.plugins(withTrace)).setDefinitionName("R2A03Core")
}

object R2A03Verilog extends App {
  SpinalConfig(targetDirectory = "gen").generateVerilog(R2A03Core(withTrace = false))
}
