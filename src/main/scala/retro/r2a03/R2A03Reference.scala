package retro.r2a03

import retro.sim.BusAccess

import scala.collection.mutable.ArrayBuffer

/** What the reference model's CPU is attached to. Each call is one CPU cycle. */
trait R2A03Bus {
  def read(address: Int): Int
  def write(address: Int, value: Int): Unit
}

object R2A03Bus {
  def memory(mem: Array[Int]): R2A03Bus = new R2A03Bus {
    def read(address: Int) = mem(address)
    def write(address: Int, value: Int) = mem(address) = value
  }
}

/** Cycle-by-cycle 2A03 reference model, written independently of the opcode table in
  * `Ricoh2A03`, for cosimulation and as the CPU of the NES system model. Every cycle is one bus
  * access on `bus`.
  *
  * `step()` runs the current instruction from the cycle after its opcode fetch up to and
  * including the fetch of the next one, which is how the RTL trace reports an instruction, and
  * returns its bus accesses. The first `step()` after `reset()` runs the reset sequence.
  *
  * Interrupts: `lines(k)` gives the (NMI, IRQ) request levels, active high, during cycle `k`
  * (counting cycles from reset, the first reset cycle being 0). Polling follows the RTL and the
  * 6502: each cycle registers a poll (an NMI edge seen so far, or IRQ with I clear, with I as it
  * is in that cycle) and a fetch acts on the poll of its second-to-last cycle; branches do not
  * poll in their operand read. */
class R2A03Reference(bus: R2A03Bus, lines: Long => (Boolean, Boolean) = _ => (false, false)) {
  def this(mem: Array[Int]) = this(R2A03Bus.memory(mem))

  var a, x, y, pc = 0
  var p = 0x20
  var s = 0
  private var ir = Ricoh2A03.RESET
  var cycle = 0L
  /** The opcode now executing (0x100 dispatch, 0x101 reset). */
  def opcode = ir
  /** The address the current instruction's opcode was fetched from. */
  var fetchAddress = 0
  private val log = ArrayBuffer[BusAccess]()

  // Interrupt state, as the RTL registers it.
  private var nmiLast, nmiPending, nmiVector, polled, decided = false

  def reset(): Unit = { ir = Ricoh2A03.RESET; p = (p & ~0x10) | 0x20; nmiLast = false; nmiPending = false; nmiVector = false; polled = false; decided = false }

  private def access(addr: Int, write: Boolean, data: Int, select: Boolean = false, hold: Boolean = false, clear: Boolean = false): Int = {
    val address = addr & 0xFFFF
    val v = if (write) { bus.write(address, data & 0xFF); data & 0xFF } else bus.read(address)
    log += BusAccess(write, address, v)
    // Sampled after the access: in a system model the bus call is where the rest of the
    // machine advances through this cycle.
    val (nmi, irq) = lines(cycle)
    // End of the cycle: an NMI edge seen in this cycle already counts for its poll and for the
    // vector choice; I is as the cycle started.
    val pending = nmiPending || (nmi && !nmiLast)
    decided = polled
    if (clear) polled = false
    else if (!hold) polled = pending || (irq && (p & 0x04) == 0)
    if (select) nmiVector = pending
    nmiPending = pending && !select
    nmiLast = nmi
    cycle += 1
    v
  }
  /** A cycle in which the CPU is halted (RDY low, a DMA has the bus): the NMI edge detector
    * still runs, nothing is polled. The system model calls it for every cycle it steals. */
  def haltedCycle(): Unit = {
    val (nmi, _) = lines(cycle)
    if (nmi && !nmiLast) nmiPending = true
    nmiLast = nmi
    cycle += 1
  }
  private def rd(addr: Int, hold: Boolean = false, clear: Boolean = false) = access(addr, write = false, 0, hold = hold, clear = clear)
  private def wr(addr: Int, v: Int, select: Boolean = false) = access(addr, write = true, v, select = select)
  private def imm(hold: Boolean = false) = { val v = rd(pc, hold); pc = (pc + 1) & 0xFFFF; v }
  private def stack = 0x100 | s
  private def push(v: Int, select: Boolean = false) = { wr(stack, v, select); s = (s - 1) & 0xFF }

  private def flag(bit: Int) = (p >> bit) & 1
  private def setFlag(bit: Int, on: Boolean) = p = if (on) p | (1 << bit) else p & ~(1 << bit)
  private def nz(v: Int) = { setFlag(7, (v & 0x80) != 0); setFlag(1, (v & 0xFF) == 0); v & 0xFF }
  private def adc(v: Int) = {
    val r = a + v + flag(0)
    setFlag(6, (~(a ^ v) & (a ^ r) & 0x80) != 0); setFlag(0, r > 0xFF)
    a = nz(r)
  }
  private def compare(reg: Int, v: Int) = { setFlag(0, reg >= v); nz(reg - v) }
  private def asl(v: Int) = { setFlag(0, (v & 0x80) != 0); nz(v << 1) }
  private def lsr(v: Int) = { setFlag(0, (v & 1) != 0); nz(v >> 1) }
  private def rol(v: Int) = { val c = flag(0); setFlag(0, (v & 0x80) != 0); nz(v << 1 | c) }
  private def ror(v: Int) = { val c = flag(0); setFlag(0, (v & 1) != 0); nz(v >> 1 | c << 7) }

  // ---- Effective addresses: each consumes exactly the cycles of the addressing mode ----------
  private def zpIdx(i: Int) = { val b = imm(); rd(b); (b + i) & 0xFF }
  private def abs() = { val lo = imm(); val hi = imm(); hi << 8 | lo }
  /** Indexed absolute: the dummy read at the uncarried address happens when the low byte carries,
    * or always for stores and read-modify-writes. */
  private def indexed(base: Int, i: Int, always: Boolean) = {
    val t = (base + i) & 0xFFFF
    if (always || (t & 0xFF00) != (base & 0xFF00)) rd((base & 0xFF00) | (t & 0xFF))
    t
  }
  private def pointer(z: Int) = { val lo = rd(z); val hi = rd((z + 1) & 0xFF); hi << 8 | lo }
  private def indX() = { val b = imm(); rd(b); pointer((b + x) & 0xFF) }

  /** The address for mode `m` (the cc = 01 encoding: 0 (zp,X), 1 zp, 3 abs, 4 (zp),Y, 5 zp,X,
    * 6 abs,Y, 7 abs,X), with zp,X / abs,X indexing by `alt` when given (SAX, LAX, STX, LDX). */
  private def address(m: Int, always: Boolean, alt: Option[Int] = None): Int = {
    val ix = alt.getOrElse(x)
    m match {
      case 0 => indX()
      case 1 => imm()
      case 3 => abs()
      case 4 => indexed(pointer(imm()), y, always)
      case 5 => zpIdx(ix)
      case 6 => indexed(abs(), y, always)
      case 7 => indexed(abs(), ix, always)
    }
  }
  private def readOperand(m: Int, alt: Option[Int] = None) = if (m == 2) imm() else rd(address(m, always = false, alt))
  private def modify(m: Int, f: Int => Int): Unit = {
    val ad = address(m, always = true)
    val v = rd(ad)
    wr(ad, v)
    wr(ad, f(v))
  }

  private def branch(taken: Boolean) = {
    val off = imm(hold = true)
    if (taken) {
      rd(pc)
      val t = (pc + (off.toByte: Int)) & 0xFFFF
      if ((t & 0xFF00) != (pc & 0xFF00)) rd((pc & 0xFF00) | (t & 0xFF))
      pc = t
    }
  }

  private def interrupt(pushed: Int) = {
    // The vector is chosen as PCL is pushed, and the sequence does not poll: the handler's
    // first instruction always runs before another interrupt.
    push(pc >> 8); push(pc & 0xFF, select = true); push(pushed)
    setFlag(2, true)
    val v = if (nmiVector) 0xFFFA else 0xFFFE
    val lo = rd(v, clear = true); val hi = rd(v + 1, clear = true)
    pc = hi << 8 | lo
  }
  private def sh(base: Int, i: Int, value: Int) = {
    val t = (base + i) & 0xFFFF
    rd((base & 0xFF00) | (t & 0xFF))
    val data = value & (((base >> 8) + 1) & 0xFF)
    wr(if ((t & 0xFF00) != (base & 0xFF00)) data << 8 | (t & 0xFF) else t, data)
  }

  private def execute(op: Int): Unit = {
    val aaa = op >> 5; val bbb = (op >> 2) & 7; val cc = op & 3
    op match {
      // ---- Interrupts, reset, BRK -----------------------------------------------------------
      case Ricoh2A03.RESET =>
        rd(pc); rd(pc)
        for (_ <- 0 until 3) { rd(stack); s = (s - 1) & 0xFF }
        setFlag(2, true)
        val lo = rd(0xFFFC); val hi = rd(0xFFFD); pc = hi << 8 | lo
      case Ricoh2A03.DISPATCH => pc = (pc - 1) & 0xFFFF; rd(pc); interrupt((p & 0xEF) | 0x20)
      case 0x00 => imm(); interrupt(p | 0x30)
      // ---- Control flow -----------------------------------------------------------------------
      case 0x4C => val lo = imm(); val hi = rd(pc); pc = hi << 8 | lo
      case 0x6C => val ptr = abs(); val lo = rd(ptr); val hi = rd((ptr & 0xFF00) | ((ptr + 1) & 0xFF)); pc = hi << 8 | lo
      case 0x20 => val lo = imm(); rd(stack); push(pc >> 8); push(pc & 0xFF); val hi = rd(pc); pc = hi << 8 | lo
      case 0x60 =>
        rd(pc); rd(stack); s = (s + 1) & 0xFF
        val lo = rd(stack); s = (s + 1) & 0xFF
        val hi = rd(stack); pc = hi << 8 | lo
        rd(pc); pc = (pc + 1) & 0xFFFF
      case 0x40 =>
        rd(pc); rd(stack); s = (s + 1) & 0xFF
        p = (rd(stack) & 0xCF) | 0x20; s = (s + 1) & 0xFF
        val lo = rd(stack); s = (s + 1) & 0xFF
        val hi = rd(stack); pc = hi << 8 | lo
      case _ if cc == 0 && bbb == 4 =>
        val f = Seq(7, 7, 6, 6, 0, 0, 1, 1)(aaa)
        branch(flag(f) == (aaa & 1))
      // ---- Stack ------------------------------------------------------------------------------
      case 0x48 => rd(pc); push(a)
      case 0x08 => rd(pc); push(p | 0x30)
      case 0x68 => rd(pc); rd(stack); s = (s + 1) & 0xFF; a = nz(rd(stack))
      case 0x28 => rd(pc); rd(stack); s = (s + 1) & 0xFF; p = (rd(stack) & 0xCF) | 0x20
      // ---- Implied ----------------------------------------------------------------------------
      case 0x58 => rd(pc); setFlag(2, false)
      case 0x78 => rd(pc); setFlag(2, true)
      case 0x18 | 0x38 | 0xB8 | 0xD8 | 0xF8 =>
        rd(pc)
        op match { case 0x18 => setFlag(0, false); case 0x38 => setFlag(0, true); case 0xB8 => setFlag(6, false)
                   case 0xD8 => setFlag(3, false); case 0xF8 => setFlag(3, true) }
      case 0xAA => rd(pc); x = nz(a)
      case 0xA8 => rd(pc); y = nz(a)
      case 0x8A => rd(pc); a = nz(x)
      case 0x98 => rd(pc); a = nz(y)
      case 0xBA => rd(pc); x = nz(s)
      case 0x9A => rd(pc); s = x
      case 0xE8 => rd(pc); x = nz(x + 1)
      case 0xC8 => rd(pc); y = nz(y + 1)
      case 0xCA => rd(pc); x = nz(x - 1)
      case 0x88 => rd(pc); y = nz(y - 1)
      case 0x0A => rd(pc); a = asl(a)
      case 0x2A => rd(pc); a = rol(a)
      case 0x4A => rd(pc); a = lsr(a)
      case 0x6A => rd(pc); a = ror(a)
      case 0xEA | 0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA => rd(pc)
      // JAM: the bus parks on FFFF; each step after the first is one more cycle there.
      case 0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92 | 0xB2 | 0xD2 | 0xF2 =>
        rd(pc); rd(0xFFFF); rd(0xFFFE); rd(0xFFFE); rd(0xFFFF)
        jammed = true
      // ---- Unstable stores ---------------------------------------------------------------------
      case 0x93 => val base = pointer(imm()); sh(base, y, a & x)
      case 0x9F => sh(abs(), y, a & x)
      case 0x9B => s = a & x; sh(abs(), y, a & x)
      case 0x9C => sh(abs(), x, y)
      case 0x9E => sh(abs(), y, x)
      case 0xBB => val v = rd(indexed(abs(), y, always = false)); s &= v; a = nz(s); x = a
      // ---- cc = 01: ALU group -------------------------------------------------------------------
      case _ if cc == 1 =>
        if (aaa == 4) { if (bbb == 2) imm() else wr(address(bbb, always = true), a) }
        else {
          val v = readOperand(bbb)
          aaa match {
            case 0 => a = nz(a | v); case 1 => a = nz(a & v); case 2 => a = nz(a ^ v); case 3 => adc(v)
            case 5 => a = nz(v); case 6 => compare(a, v); case 7 => adc(v ^ 0xFF)
          }
        }
      // ---- cc = 11: undocumented --------------------------------------------------------------
      case _ if cc == 3 && bbb == 2 =>
        val v = imm()
        aaa match {
          case 0 | 1 => a = nz(a & v); setFlag(0, (a & 0x80) != 0)
          case 2 => a = lsr(a & v)
          case 3 =>
            val t = a & v
            a = nz(t >> 1 | flag(0) << 7)
            setFlag(0, (a & 0x40) != 0); setFlag(6, (((a >> 6) ^ (a >> 5)) & 1) == 1)
          case 4 => a = nz((a | Alu.MAGIC) & x & v)
          case 5 => a = nz((a | Alu.MAGIC) & v); x = a
          case 6 => val t = a & x; setFlag(0, t >= v); x = nz(t - v)
          case 7 => adc(v ^ 0xFF)
        }
      case _ if cc == 3 && aaa == 4 => // SAX
        val m = if (bbb == 5) 5 else bbb
        wr(address(m, always = true, alt = Some(y)), a & x)
      case _ if cc == 3 && aaa == 5 => // LAX
        val v = readOperand(bbb, alt = Some(y)); a = nz(v); x = a
      case _ if cc == 3 =>
        modify(bbb, v => aaa match {
          case 0 => val r = asl(v); a = nz(a | r); r
          case 1 => val r = rol(v); a = nz(a & r); r
          case 2 => val r = lsr(v); a = nz(a ^ r); r
          case 3 => val r = ror(v); adc(r); r
          case 6 => val r = (v - 1) & 0xFF; compare(a, r); r
          case 7 => val r = (v + 1) & 0xFF; adc(r ^ 0xFF); r
        })
      // ---- cc = 10: shifts, INC/DEC, STX/LDX, NOPs ------------------------------------------------
      case 0xA2 => x = nz(imm())
      case _ if cc == 2 && bbb == 0 => imm() // 82 C2 E2: NOP #n
      case _ if cc == 2 && aaa == 4 => // STX: zp, abs, zp,Y
        val ad = bbb match { case 1 => imm(); case 3 => abs(); case 5 => zpIdx(y) }
        wr(ad, x)
      case _ if cc == 2 && aaa == 5 => // LDX: zp, abs, zp,Y, abs,Y
        val v = bbb match { case 1 => rd(imm()); case 3 => rd(abs()); case 5 => rd(zpIdx(y)); case 7 => rd(indexed(abs(), y, always = false)) }
        x = nz(v)
      case _ if cc == 2 =>
        val m = bbb match { case 1 => 1; case 3 => 3; case 5 => 5; case 7 => 7 }
        modify(m, v => aaa match { case 0 => asl(v); case 1 => rol(v); case 2 => lsr(v); case 3 => ror(v); case 6 => nz(v - 1); case 7 => nz(v + 1) })
      // ---- cc = 00: BIT, STY, LDY, CPY, CPX, NOPs -------------------------------------------------
      case 0x24 | 0x2C =>
        val v = rd(if (op == 0x24) imm() else abs())
        setFlag(7, (v & 0x80) != 0); setFlag(6, (v & 0x40) != 0); setFlag(1, (a & v) == 0)
      case 0x84 | 0x8C | 0x94 => wr(bbb match { case 1 => imm(); case 3 => abs(); case 5 => zpIdx(x) }, y)
      case 0xA0 | 0xA4 | 0xAC | 0xB4 | 0xBC => y = nz(readOperand(if (bbb == 0) 2 else bbb))
      case 0xC0 | 0xC4 | 0xCC => compare(y, readOperand(if (bbb == 0) 2 else bbb))
      case 0xE0 | 0xE4 | 0xEC => compare(x, readOperand(if (bbb == 0) 2 else bbb))
      case 0x80 => imm()
      case 0x04 | 0x44 | 0x64 | 0x0C | 0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 | 0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => readOperand(bbb)
    }
  }
  private var jammed = false

  /** Runs the current instruction and the fetch of the next; returns that instruction's bus
    * accesses (the fetch included) and its cycle count. */
  def step(): (Seq[BusAccess], Int) = {
    log.clear()
    val start = cycle
    if (jammed) rd(0xFFFF) else { execute(ir); if (!jammed) fetchOnly() }
    (log.toList, (cycle - start).toInt)
  }

  /** Only the opcode fetch at PC, as the start of a single-instruction test; returns its access. */
  def fetch(): BusAccess = { log.clear(); fetchOnly(); log.head }
  private def fetchOnly(): Unit = {
    fetchAddress = pc
    val take = decided
    val opcode = rd(pc)
    pc = (pc + 1) & 0xFFFF
    ir = if (take) Ricoh2A03.DISPATCH else opcode
  }

  def regs: Map[String, Int] = Map("A" -> a, "X" -> x, "Y" -> y, "SP" -> s, "P" -> p, "PCH" -> (pc >> 8), "PCL" -> (pc & 0xFF))
}
