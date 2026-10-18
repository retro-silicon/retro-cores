package retro.sm83

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random

/** Cosimulation: the RTL and `Sm83Reference` run the same memory image; every retired
  * instruction must match in address, opcode, M-cycle count, bus accesses and registers. */
class Sm83CosimSim extends AnyFunSuite {
  lazy val compiled = SimConfig.withVerilator.compile(Sm83Core())
  val implemented = Sm83.names.keys.toIndexedSeq.sorted

  /** Checks each RTL retire against the reference as it happens. */
  class CosimBackend(ref: Sm83Reference) extends TraceBackend {
    var checked = 0
    override def onRetire(e: RetireEvent) = {
      val name = Sm83.names.getOrElse(e.opcode, "?")
      val (refBus, refCycles) = ref.step()
      val ctx = f"instruction #${e.index} at ${e.address}%04X ${e.opcode}%02X $name"
      assert(e.bus == refBus, s"$ctx: bus\n  rtl ${e.bus.mkString(", ")}\n  ref ${refBus.mkString(", ")}")
      assert(e.mcycles == refCycles, s"$ctx: ${e.mcycles} M-cycles, reference says $refCycles")
      val rtlRegs = Sm83.architectural.map(n => n -> e.regs(n)).toMap
      assert(rtlRegs == ref.regs, s"$ctx: registers\n  rtl $rtlRegs\n  ref ${ref.regs}")
      checked += 1
    }
  }

  def cosim(image: Array[Int], instructions: Int, seed: Int, waitStates: Boolean) =
    compiled.doSimUntilVoid(seed = seed) { dut =>
      val io = dut.host[BusPlugin].logic.io
      val mem = image.clone()
      val ref = new Sm83Reference(image.clone())
      ref.reset()
      val cosim = new CosimBackend(ref)
      new Probe(dut.host[TracePlugin], dut.clockDomain).add(cosim)

      val rnd = new Random(seed)
      io.stall #= false
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings {
        if (io.cmd.valid.toBoolean && io.cmd.write.toBoolean && !io.stall.toBoolean) mem(io.cmd.address.toInt) = io.cmd.data.toInt
        io.stall #= waitStates && rnd.nextInt(4) == 0
      }
      fork { while (true) { io.rdata #= mem(io.cmd.address.toInt); sleep(1) } }

      dut.clockDomain.waitSamplingWhere(cosim.checked >= instructions)
      simSuccess()
    }

  test("hand-written program: copy loop with 16-bit pointers and flags") {
    // LD HL,C000 / LD DE,D000 / LD B,05 / loop: LD A,(HL+) / ADD A,B / LD (DE),A / INC DE / DEC B / JR NZ,loop
    // then: XOR A / CP 01 / JR C,+2 / ... / JP 0000
    val prog = Seq(0x21, 0x00, 0xC0, 0x11, 0x00, 0xD0, 0x06, 0x05,
      0x2A, 0x80, 0x12, 0x13, 0x05, 0x20, 0xF9,
      0xAF, 0xFE, 0x01, 0x38, 0x02, 0x3E, 0x99, 0xC3, 0x00, 0x00)
    val image = Array.fill(0x10000)(0)
    prog.zipWithIndex.foreach { case (v, i) => image(i) = v }
    for (i <- 0 until 5) image(0xC000 + i) = 0x10 * i + 0x0F
    cosim(image, 200, seed = 1, waitStates = false)
  }

  // Memory filled entirely with implemented opcodes: every byte, operand or not, decodes,
  // so random jumps and pointer writes always land on valid code.
  for ((seed, wait) <- Seq((11, false), (12, false), (13, true))) {
    test(s"random code, seed $seed${if (wait) ", random wait states" else ""}") {
      val rnd = new Random(seed)
      cosim(Array.fill(0x10000)(implemented(rnd.nextInt(implemented.size))), 5000, seed, wait)
    }
  }
}
