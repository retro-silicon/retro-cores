package retro.r2a03

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.sim._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Cosimulation: the RTL and `R2A03Reference` run the same memory image and see the same NMI and
  * IRQ levels in every cycle; every retired instruction must match in cycle count, ordered bus
  * accesses and registers. This is what covers interrupt timing (polling, hijacking, the CLI and
  * branch rules) against an independent model, on top of the single-step tests. */
class R2A03CosimSim extends AnyFunSuite {
  lazy val compiled = SimConfig.withVerilator.compile(R2A03Core())
  val jams = Set(0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2)
  val opcodes = (0 until 256).filterNot(jams).toIndexedSeq

  class CosimBackend(ref: R2A03Reference) extends TraceBackend {
    var checked = 0
    var resetDone = false
    val seen = collection.mutable.Map[Int, Int]().withDefaultValue(0)
    override def onRetire(e: RetireEvent) = try check(e) catch { case t: Throwable => failure = Some(t) }
    var failure: Option[Throwable] = None
    var jammed = false
    def check(e: RetireEvent) = {
      // The model's first step is the reset sequence, which the probe does not report.
      if (!resetDone) { ref.step(); resetDone = true }
      val opcode = ref.opcode
      val (bus, cycles) = ref.step()
      val ctx = f"instruction #${e.index} at ${e.address}%04X ${e.opcode}%03X ${Ricoh2A03.names.getOrElse(e.opcode, "?")}"
      assert(e.opcode == opcode, f"$ctx: the model runs $opcode%03X")
      assert(e.bus == bus, s"$ctx: bus\n  rtl ${e.bus.mkString(", ")}\n  ref ${bus.mkString(", ")}")
      assert(e.mcycles == cycles, s"$ctx: ${e.mcycles} cycles, reference says $cycles")
      val rtl = R2A03Regs.architectural.map(r => r.name -> e.regs(r.name).toInt).toMap
      assert(rtl == ref.regs, s"$ctx: registers\n  rtl $rtl\n  ref ${ref.regs}")
      seen(e.opcode) += 1
      checked += 1
      // Stores can write a JAM opcode into the code; the run ends there.
      if (jams(ref.opcode)) jammed = true
    }
  }

  /** @param irqToggle chance per cycle that the IRQ level flips
    * @param nmiToggle chance per cycle that the NMI level flips */
  def cosim(image: Array[Int], instructions: Int, seed: Int, irqToggle: Double, nmiToggle: Double): Map[Int, Int] = {
    var seen: Map[Int, Int] = null
    compiled.doSimUntilVoid(seed = seed) { dut =>
      val io = dut.host[BusPlugin].logic.io
      val int = dut.host[Ricoh2A03.InterruptPlugin].logic
      val mem = image.clone()
      val lineLog = ArrayBuffer[(Boolean, Boolean)]()
      val ref = new R2A03Reference(R2A03Bus.memory(image.clone()), k => lineLog(k.toInt))
      val cosim = new CosimBackend(ref)
      val rnd = new Random(seed)
      var nmi, irq = false
      io.stall #= false
      int.nmi #= false
      int.irq #= false
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings {
        if (!dut.clockDomain.isResetAsserted) {
          lineLog += (int.nmi.toBoolean -> int.irq.toBoolean)
          if (io.cmd.write.toBoolean) mem(io.cmd.address.toInt) = io.cmd.data.toInt
          if (rnd.nextDouble() < irqToggle) irq = !irq
          if (rnd.nextDouble() < nmiToggle) nmi = !nmi
          int.irq #= irq
          int.nmi #= nmi
        }
      }
      new Probe(dut.host[TracePlugin], dut.clockDomain).add(cosim)
      fork { while (true) { io.rdata #= mem(io.cmd.address.toInt); sleep(1) } }
      var cycles = 0L
      dut.clockDomain.onSamplings(cycles += 1)
      dut.clockDomain.waitSamplingWhere(cosim.checked >= instructions || cosim.failure.nonEmpty || cosim.jammed || cycles > instructions * 20L)
      assert(cosim.checked >= instructions || cosim.failure.nonEmpty || cosim.jammed, s"only ${cosim.checked} instructions retired in $cycles cycles")
      for (t <- cosim.failure) throw t
      seen = cosim.seen.toMap
      simSuccess()
    }
    seen
  }

  def image(bytes: (Int, Seq[Int])*) = {
    val m = Array.fill(0x10000)(0xEA)
    for ((base, bs) <- bytes; (v, i) <- bs.zipWithIndex) m(base + i) = v
    m
  }

  test("hand-written program: loop with indexed stores, JSR and RTS") {
    // 8000 LDX #8; loop: TXA; STA 0200,X; JSR 9000; DEX; BNE loop; JMP 8000.  9000: INC 10; RTS
    val prog = Seq(0xA2, 0x08, 0x8A, 0x9D, 0x00, 0x02, 0x20, 0x00, 0x90, 0xCA, 0xD0, 0xF6, 0x4C, 0x00, 0x80)
    cosim(image(0x8000 -> prog, 0x9000 -> Seq(0xE6, 0x10, 0x60), 0xFFFC -> Seq(0x00, 0x80)), 300, seed = 1, 0, 0)
  }

  // Random memory of non-JAM opcodes: every byte decodes, so jumps, returns and vectors always
  // land on code. Coverage comes from many short runs.
  for ((name, irq, nmi) <- Seq(("no interrupts", 0.0, 0.0), ("IRQ", 0.02, 0.0), ("IRQ and NMI", 0.03, 0.01))) {
    test(s"random code, $name") {
      val seen = collection.mutable.Map[Int, Int]().withDefaultValue(0)
      for (seed <- 0 until 30) {
        val rnd = new Random(seed * 7919 + name.length)
        val mem = Array.fill(0x10000)(opcodes(rnd.nextInt(opcodes.size)))
        for ((o, n) <- cosim(mem, 2000, seed, irq, nmi)) seen(o) += n
      }
      val base = seen.keys.count(_ < 0x100)
      println(s"random code, $name: ${seen.values.sum} instructions, $base/${opcodes.size} opcodes, ${seen(Ricoh2A03.DISPATCH)} interrupts")
      assert(base >= opcodes.size * 9 / 10, s"only $base opcodes exercised")
      if (irq > 0) assert(seen(Ricoh2A03.DISPATCH) > 20)
    }
  }
}
