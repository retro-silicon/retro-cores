package retro.sm83

import retro.sim.BusAccess

import scala.collection.mutable.ArrayBuffer

/** What the reference model's CPU is attached to. Each call is one M-cycle. */
trait Sm83Bus {
  def read(address: Int): Int
  def write(address: Int, value: Int): Unit
  def idle(): Unit = {}
  /** Called just before the read, write or idle of an M-cycle in which the IDU (the 16-bit
    * incrementer on the address side) increments or decrements `address`, in the same M-cycles as
    * the RTL microcode. The DMG OAM corruption bug depends on it. */
  def idu(address: Int): Unit = {}
}

object Sm83Bus {
  /** Plain memory: the CPU alone, as the bare-core cosim sees it. */
  def memory(mem: Array[Int]): Sm83Bus = new Sm83Bus {
    def read(address: Int) = mem(address)
    def write(address: Int, value: Int) = mem(address) = value
  }
}

/** Instruction-level SM83 reference model, written independently of the opcode table in `Sm83`,
  * for cosimulation. It logs every bus access and counts M-cycles, including the overlapped fetch
  * of the next opcode at the end of each instruction, which is how the RTL trace reports them.
  *
  * Interrupts: `requests(k)` must return IE & IF as it was during M-cycle `k` (counting
  * completed M-cycles from reset, the reset fetch being cycle 0). The model decides by itself
  * when to dispatch, which vector to take and how long HALT waits, and calls `ack(index)` at the
  * end of the dispatch cycle that pushes the low byte of PC, where the vector is chosen. */
class Sm83Reference(bus: Sm83Bus, requests: Long => Int, ack: Int => Unit) {
  def this(mem: Array[Int], requests: Long => Int) = this(Sm83Bus.memory(mem), requests, _ => ())

  var a, f, b, c, d, e, h, l = 0
  var sp, pc = 0
  var ime, imeNext = false
  private var ir = 0
  def halted = ir == Sm83.HALTED
  /** The opcode (or `Sm83.DISPATCH` / `Sm83.HALTED`) the next `step` executes. */
  def opcode: Int = ir
  private var cycle = 0L
  private val log = ArrayBuffer[BusAccess]()

  private def read(addr: Int) = { cycle += 1; val v = bus.read(addr & 0xFFFF); log += BusAccess(false, addr & 0xFFFF, v); v }
  private def write(addr: Int, v: Int) = { cycle += 1; bus.write(addr & 0xFFFF, v & 0xFF); log += BusAccess(true, addr & 0xFFFF, v & 0xFF) }
  private def imm() = { bus.idu(pc); val v = read(pc); pc = (pc + 1) & 0xFFFF; v }
  private def idle() = { cycle += 1; bus.idle() }

  private def hl = h << 8 | l
  private def setHl(v: Int) = { h = (v >> 8) & 0xFF; l = v & 0xFF }
  private def zf = (f >> 7) & 1
  private def nf = (f >> 6) & 1
  private def hf = (f >> 5) & 1
  private def cf = (f >> 4) & 1
  private def flags(z: Boolean, n: Boolean, hc: Boolean, cy: Boolean) =
    f = (if (z) 0x80 else 0) | (if (n) 0x40 else 0) | (if (hc) 0x20 else 0) | (if (cy) 0x10 else 0)

  private def getR(i: Int): Int = i match {
    case 0 => b; case 1 => c; case 2 => d; case 3 => e; case 4 => h; case 5 => l; case 6 => read(hl); case 7 => a
  }
  private def setR(i: Int, v: Int): Unit = i match {
    case 0 => b = v & 0xFF; case 1 => c = v & 0xFF; case 2 => d = v & 0xFF; case 3 => e = v & 0xFF
    case 4 => h = v & 0xFF; case 5 => l = v & 0xFF; case 6 => write(hl, v); case 7 => a = v & 0xFF
  }
  private def getRR(i: Int) = i match { case 0 => b << 8 | c; case 1 => d << 8 | e; case 2 => hl; case 3 => sp }
  private def setRR(i: Int, v: Int) = i match {
    case 0 => b = (v >> 8) & 0xFF; c = v & 0xFF
    case 1 => d = (v >> 8) & 0xFF; e = v & 0xFF
    case 2 => setHl(v)
    case 3 => sp = v & 0xFFFF
  }
  private def cond(i: Int) = i match { case 0 => zf == 0; case 1 => zf == 1; case 2 => cf == 0; case 3 => cf == 1 }

  private def push16(v: Int) = {
    bus.idu(sp); idle(); sp = (sp - 1) & 0xFFFF
    bus.idu(sp); write(sp, v >> 8); sp = (sp - 1) & 0xFFFF
    write(sp, v & 0xFF)
  }
  private def pop16() = {
    bus.idu(sp); val lo = read(sp); sp = (sp + 1) & 0xFFFF
    bus.idu(sp); val hi = read(sp); sp = (sp + 1) & 0xFFFF
    hi << 8 | lo
  }

  private def alu(op: Int, v: Int): Unit = {
    val carry = if (op == 1 || op == 3) cf else 0
    op match {
      case 0 | 1 =>
        val r = a + v + carry
        flags((r & 0xFF) == 0, false, (a & 0xF) + (v & 0xF) + carry > 0xF, r > 0xFF); a = r & 0xFF
      case 2 | 3 | 7 =>
        val r = a - v - carry
        flags((r & 0xFF) == 0, true, (a & 0xF) - (v & 0xF) - carry < 0, r < 0)
        if (op != 7) a = r & 0xFF
      case 4 => a &= v; flags(a == 0, false, true, false)
      case 5 => a ^= v; flags(a == 0, false, false, false)
      case 6 => a |= v; flags(a == 0, false, false, false)
    }
  }

  /** CB-page and accumulator rotates. Returns (result, carry out). */
  private def shift(op: Int, v: Int): (Int, Boolean) = op match {
    case 0 => (((v << 1) | (v >> 7)) & 0xFF, (v & 0x80) != 0)
    case 1 => (((v >> 1) | (v << 7)) & 0xFF, (v & 1) != 0)
    case 2 => (((v << 1) | cf) & 0xFF, (v & 0x80) != 0)
    case 3 => ((v >> 1) | (cf << 7), (v & 1) != 0)
    case 4 => ((v << 1) & 0xFF, (v & 0x80) != 0)
    case 5 => ((v >> 1) | (v & 0x80), (v & 1) != 0)
    case 6 => (((v << 4) | (v >> 4)) & 0xFF, false)
    case 7 => (v >> 1, (v & 1) != 0)
  }

  private def addSpE(): Int = {
    val e8 = imm()
    flags(false, false, (sp & 0xF) + (e8 & 0xF) > 0xF, (sp & 0xFF) + e8 > 0xFF)
    (sp + e8.toByte) & 0xFFFF
  }

  // How the instruction's final fetch treats IME (key IME_OP in the RTL).
  private val NONE = 0; private val DI = 1; private val EI = 2; private val RETI = 3

  /** The instruction-ending fetch, with the interrupt check. HALT's fetch (`halt`) keeps the CPU
    * halted, PC unchanged, while nothing is pending. */
  private def fetch(imeOp: Int = NONE, increment: Boolean = true, halt: Boolean = false): Unit = {
    val k = cycle
    if (increment && !halt) bus.idu(pc)
    val opcode = read(pc)
    val r = requests(k)
    // Bits 8-12 are requests a halted CPU does not see yet, bits 16-20 ones a running CPU does not.
    val reqs = if (halt) r & 0x1F & ~(r >> 8) else r & 0x1F & ~(r >> 16)
    if (increment && !(halt && reqs == 0)) pc = (pc + 1) & 0xFFFF
    val effective = imeOp == RETI || ((ime || imeNext) && imeOp != DI)
    val dispatch = effective && reqs != 0
    ime = effective && !dispatch
    imeNext = imeOp == EI && !dispatch
    ir = if (dispatch) Sm83.DISPATCH else if (halt && reqs == 0) Sm83.HALTED else opcode
  }

  /** Call once after reset: the reset NOP's fetch of the first opcode (M-cycle 0). */
  def reset(): Unit = fetch()

  /** Executes one instruction (CB-prefixed ones whole). Returns its bus accesses and M-cycles. */
  def step(): (Seq[BusAccess], Int) = {
    log.clear()
    val start = cycle
    var imeOp = NONE
    var increment = true
    var halt = false
    ir match {
      case Sm83.DISPATCH =>
        bus.idu(pc); idle(); pc = (pc - 1) & 0xFFFF
        bus.idu(sp); idle(); sp = (sp - 1) & 0xFFFF; bus.idu(sp); write(sp, pc >> 8); sp = (sp - 1) & 0xFFFF
        val k = cycle
        write(sp, pc & 0xFF)
        // The push of the high byte may have changed IE: the vector is chosen now.
        val reqs = requests(k) & 0x1F
        pc = if (reqs == 0) 0 else { val i = Integer.numberOfTrailingZeros(reqs); ack(i); 0x40 + 8 * i }
      case 0x00 | 0x10 => // NOP; STOP is not modelled and runs as a NOP, as in the RTL
      case 0x76 =>
        idle()
        // Entered with an interrupt pending, HALT does not wait and its fetch does not increment
        // PC: with IME off the next byte runs twice (the HALT bug), with IME on the dispatch
        // returns to the HALT itself (AGE ei-halt, halt-m0-interrupt).
        if ((requests(cycle - 1) & 0x1F) != 0) increment = false else halt = true
      case Sm83.HALTED => halt = true
      case o if (o & 0xC0) == 0x40 => setR((o >> 3) & 7, getR(o & 7))
      case o if (o & 0xC7) == 0x06 => setR((o >> 3) & 7, imm())
      case o if (o & 0xCF) == 0x01 => val lo = imm(); val hi = imm(); setRR(o >> 4, hi << 8 | lo)
      case o if (o & 0xCF) == 0x03 => bus.idu(getRR(o >> 4)); idle(); setRR(o >> 4, getRR(o >> 4) + 1)
      case o if (o & 0xCF) == 0x0B => bus.idu(getRR(o >> 4)); idle(); setRR(o >> 4, getRR(o >> 4) - 1)
      case o if (o & 0xCF) == 0x09 =>
        idle(); val v = getRR(o >> 4)
        flags(zf == 1, false, (hl & 0xFFF) + (v & 0xFFF) > 0xFFF, hl + v > 0xFFFF); setHl(hl + v)
      case 0x02 => write(getRR(0), a)
      case 0x12 => write(getRR(1), a)
      case 0x0A => a = read(getRR(0))
      case 0x1A => a = read(getRR(1))
      case 0x22 => bus.idu(hl); write(hl, a); setHl(hl + 1)
      case 0x32 => bus.idu(hl); write(hl, a); setHl(hl - 1)
      case 0x2A => bus.idu(hl); a = read(hl); setHl(hl + 1)
      case 0x3A => bus.idu(hl); a = read(hl); setHl(hl - 1)
      case 0xE0 => val n = imm(); write(0xFF00 | n, a)
      case 0xF0 => val n = imm(); a = read(0xFF00 | n)
      case 0xE2 => write(0xFF00 | c, a)
      case 0xF2 => a = read(0xFF00 | c)
      case 0xEA => val lo = imm(); val hi = imm(); write(hi << 8 | lo, a)
      case 0xFA => val lo = imm(); val hi = imm(); a = read(hi << 8 | lo)
      case 0x08 => val lo = imm(); val hi = imm(); val nn = hi << 8 | lo; bus.idu(nn); write(nn, sp & 0xFF); write(nn + 1, sp >> 8)
      case 0xF9 => idle(); sp = hl
      case 0xE8 => sp = addSpE(); idle(); idle()
      case 0xF8 => setHl(addSpE()); idle()
      case o if (o & 0xCF) == 0xC5 => push16(if (o == 0xF5) a << 8 | f else getRR((o >> 4) & 3))
      case o if (o & 0xCF) == 0xC1 =>
        val v = pop16()
        if (o == 0xF1) { a = v >> 8; f = v & 0xF0 } else setRR((o >> 4) & 3, v)
      case o if (o & 0xC7) == 0x04 =>
        val r = (o >> 3) & 7; val v = getR(r); val n = (v + 1) & 0xFF
        f = (f & 0x10) | (if (n == 0) 0x80 else 0) | (if ((v & 0xF) == 0xF) 0x20 else 0); setR(r, n)
      case o if (o & 0xC7) == 0x05 =>
        val r = (o >> 3) & 7; val v = getR(r); val n = (v - 1) & 0xFF
        f = (f & 0x10) | (if (n == 0) 0x80 else 0) | 0x40 | (if ((v & 0xF) == 0) 0x20 else 0); setR(r, n)
      case o if (o & 0xC0) == 0x80 => alu((o >> 3) & 7, getR(o & 7))
      case o if (o & 0xC7) == 0xC6 => alu((o >> 3) & 7, imm())
      case 0x27 =>
        var corr = 0; var carry = cf == 1
        if (hf == 1 || (nf == 0 && (a & 0xF) > 9)) corr |= 0x06
        if (carry || (nf == 0 && a > 0x99)) { corr |= 0x60; carry = true }
        a = (if (nf == 1) a - corr else a + corr) & 0xFF
        flags(a == 0, nf == 1, false, carry)
      case 0x2F => a ^= 0xFF; f = (f & 0x90) | 0x60
      case 0x37 => f = (f & 0x80) | 0x10
      case 0x3F => f = (f & 0x80) | ((f & 0x10) ^ 0x10)
      case o if (o & 0xE7) == 0x07 =>
        val (r, co) = shift((o >> 3) & 3, a); a = r; flags(false, false, false, co)
      case 0x18 => val e8 = imm().toByte; idle(); pc = (pc + e8) & 0xFFFF
      case o if (o & 0xE7) == 0x20 => val e8 = imm().toByte; if (cond((o >> 3) & 3)) { idle(); pc = (pc + e8) & 0xFFFF }
      case 0xC3 => val lo = imm(); val hi = imm(); idle(); pc = hi << 8 | lo
      case o if (o & 0xE7) == 0xC2 => val lo = imm(); val hi = imm(); if (cond((o >> 3) & 3)) { idle(); pc = hi << 8 | lo }
      case 0xE9 => pc = hl
      case 0xCD => val lo = imm(); val hi = imm(); push16(pc); pc = hi << 8 | lo
      case o if (o & 0xE7) == 0xC4 => val lo = imm(); val hi = imm(); if (cond((o >> 3) & 3)) { push16(pc); pc = hi << 8 | lo }
      case 0xC9 => pc = pop16(); idle()
      case 0xD9 => pc = pop16(); idle(); imeOp = RETI
      case o if (o & 0xE7) == 0xC0 => idle(); if (cond((o >> 3) & 3)) { pc = pop16(); idle() }
      case o if (o & 0xC7) == 0xC7 => push16(pc); pc = o & 0x38
      case 0xF3 => imeOp = DI
      case 0xFB => imeOp = EI
      case 0xCB =>
        val x = imm()
        val (r, y) = (x & 7, (x >> 3) & 7)
        val v = getR(r)
        x >> 6 match {
          case 0 => val (res, co) = shift(y, v); flags(res == 0, false, false, co); setR(r, res)
          case 1 => f = (f & 0x10) | 0x20 | (if ((v >> y & 1) == 0) 0x80 else 0)
          case 2 => setR(r, v & ~(1 << y))
          case 3 => setR(r, v | (1 << y))
        }
      case o if Sm83.unused.contains(o) =>
        // The CPU locks up: an idle M-cycle forever, no interrupts.
        idle()
        return (log.toList, (cycle - start).toInt)
      case _ =>
    }
    fetch(imeOp, increment, halt)
    (log.toList, (cycle - start).toInt)
  }

  /** Start from a given register state instead of all zeros (a console's post-boot state). */
  def load(state: Map[String, Int]): Unit = for ((k, v) <- state) k match {
    case "A" => a = v; case "F" => f = v; case "B" => b = v; case "C" => c = v; case "D" => d = v; case "E" => e = v
    case "H" => h = v; case "L" => l = v; case "SPH" => sp = (sp & 0xFF) | v << 8; case "SPL" => sp = (sp & 0xFF00) | v
    case "PC" => pc = v; case "W" | "Z" =>
  }

  def regs: Map[String, BigInt] = Map("A" -> a, "F" -> f, "B" -> b, "C" -> c, "D" -> d, "E" -> e, "H" -> h, "L" -> l,
    "SPH" -> (sp >> 8), "SPL" -> (sp & 0xFF), "PC" -> pc).map { case (k, v) => k -> BigInt(v) }
}
