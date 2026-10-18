package retro.sm83

import retro.sim.BusAccess

import scala.collection.mutable.ArrayBuffer

/** Instruction-level SM83 reference model, written independently of the opcode table in `Sm83`,
  * for cosimulation. It logs every bus access and counts M-cycles, including the overlapped fetch
  * of the next opcode at the end of each instruction, which is how the RTL trace reports them.
  * Covers the same subset as the RTL. */
class Sm83Reference(val mem: Array[Int]) {
  var a, f, b, c, d, e, h, l = 0
  var sp, pc = 0
  private var ir = 0
  private val log = ArrayBuffer[BusAccess]()
  private var internal = 0

  private def read(addr: Int) = { val v = mem(addr & 0xFFFF); log += BusAccess(false, addr & 0xFFFF, v); v }
  private def write(addr: Int, v: Int) = { mem(addr & 0xFFFF) = v & 0xFF; log += BusAccess(true, addr & 0xFFFF, v & 0xFF) }
  private def imm() = { val v = read(pc); pc = (pc + 1) & 0xFFFF; v }
  private def idle() = internal += 1

  def hl = h << 8 | l
  private def setHl(v: Int) = { h = (v >> 8) & 0xFF; l = v & 0xFF }
  private def zf = (f >> 7) & 1
  private def cf = (f >> 4) & 1
  private def flags(z: Boolean, n: Boolean, hc: Boolean, cy: Boolean) =
    f = (if (z) 0x80 else 0) | (if (n) 0x40 else 0) | (if (hc) 0x20 else 0) | (if (cy) 0x10 else 0)

  private def getR(i: Int): Int = i match {
    case 0 => b; case 1 => c; case 2 => d; case 3 => e; case 4 => h; case 5 => l; case 6 => read(hl); case 7 => a
  }
  private def setR(i: Int, v: Int): Unit = i match {
    case 0 => b = v; case 1 => c = v; case 2 => d = v; case 3 => e = v; case 4 => h = v; case 5 => l = v
    case 6 => write(hl, v); case 7 => a = v
  }
  private def getRR(i: Int) = i match { case 0 => b << 8 | c; case 1 => d << 8 | e; case 2 => hl; case 3 => sp }
  private def setRR(i: Int, v: Int) = i match {
    case 0 => b = (v >> 8) & 0xFF; c = v & 0xFF
    case 1 => d = (v >> 8) & 0xFF; e = v & 0xFF
    case 2 => setHl(v)
    case 3 => sp = v & 0xFFFF
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

  /** Call once after reset: fetches the first opcode, as the RTL's reset NOP does. */
  def reset(): Unit = { ir = mem(pc); pc = (pc + 1) & 0xFFFF }

  /** Executes one instruction. Returns its bus accesses and M-cycle count. */
  def step(): (Seq[BusAccess], Int) = {
    log.clear(); internal = 0
    val op = ir
    op match {
      case 0x00 =>
      case 0x76 => sys.error("HALT not modelled")
      case o if (o & 0xC0) == 0x40 => setR((o >> 3) & 7, getR(o & 7))
      case o if (o & 0xC7) == 0x06 => setR((o >> 3) & 7, imm())
      case o if (o & 0xCF) == 0x01 => val lo = imm(); val hi = imm(); setRR(o >> 4, hi << 8 | lo)
      case o if (o & 0xCF) == 0x03 => idle(); setRR(o >> 4, getRR(o >> 4) + 1)
      case o if (o & 0xCF) == 0x0B => idle(); setRR(o >> 4, getRR(o >> 4) - 1)
      case 0x02 => write(getRR(0), a)
      case 0x12 => write(getRR(1), a)
      case 0x0A => a = read(getRR(0))
      case 0x1A => a = read(getRR(1))
      case 0x22 => write(hl, a); setHl(hl + 1)
      case 0x32 => write(hl, a); setHl(hl - 1)
      case 0x2A => a = read(hl); setHl(hl + 1)
      case 0x3A => a = read(hl); setHl(hl - 1)
      case o if (o & 0xC7) == 0x04 =>
        val r = (o >> 3) & 7; val v = getR(r); val n = (v + 1) & 0xFF
        f = (f & 0x10) | (if (n == 0) 0x80 else 0) | (if ((v & 0xF) == 0xF) 0x20 else 0); setR(r, n)
      case o if (o & 0xC7) == 0x05 =>
        val r = (o >> 3) & 7; val v = getR(r); val n = (v - 1) & 0xFF
        f = (f & 0x10) | (if (n == 0) 0x80 else 0) | 0x40 | (if ((v & 0xF) == 0) 0x20 else 0); setR(r, n)
      case o if (o & 0xC0) == 0x80 => alu((o >> 3) & 7, getR(o & 7))
      case o if (o & 0xC7) == 0xC6 => alu((o >> 3) & 7, imm())
      case 0x18 => val e8 = imm().toByte; idle(); pc = (pc + e8) & 0xFFFF
      case o if (o & 0xE7) == 0x20 =>
        val e8 = imm().toByte
        val taken = ((o >> 3) & 3) match { case 0 => zf == 0; case 1 => zf == 1; case 2 => cf == 0; case 3 => cf == 1 }
        if (taken) { idle(); pc = (pc + e8) & 0xFFFF }
      case 0xC3 => val lo = imm(); val hi = imm(); idle(); pc = hi << 8 | lo
      case o => sys.error(f"opcode $o%02X not modelled")
    }
    ir = imm() // overlapped fetch of the next opcode
    (log.toList, log.size + internal)
  }

  def regs: Map[String, BigInt] = Map("A" -> a, "F" -> f, "B" -> b, "C" -> c, "D" -> d, "E" -> e, "H" -> h, "L" -> l,
    "SPH" -> (sp >> 8), "SPL" -> (sp & 0xFF), "PC" -> pc).map { case (k, v) => k -> BigInt(v) }
}
