package retro.r2a03

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

/** Directed checks of IRQ and NMI dispatch, which the single-instruction tests do not cover: the
  * bus sequence of the dispatch, B clear in the pushed P, the one-instruction delay after CLI,
  * NMI on a rising edge only, and the extra instruction a taken same-page branch lets through.
  * Expected timings follow the nesdev wiki ("CPU interrupts"). */
class R2A03InterruptSim extends AnyFunSuite {
  lazy val compiled = SimConfig.withVerilator.compile(R2A03Core())

  case class Access(write: Boolean, address: Int, data: Int) {
    override def toString = f"${if (write) "w" else "r"} $address%04X $data%02X"
  }

  /** Runs `program` (loaded at 8000, vectors at FFFA..FFFF) and returns every bus access with the
    * fetch flag. `irqAt(cycle)` and `nmiAt(cycle)` give the line levels per cycle after reset. */
  def run(program: Map[Int, Seq[Int]], cycles: Int, irqAt: Int => Boolean = _ => false, nmiAt: Int => Boolean = _ => false): Seq[(Access, Boolean)] = {
    val mem = Array.fill(0x10000)(0xEA)
    for ((at, bytes) <- program; (b, i) <- bytes.zipWithIndex) mem(at + i) = b
    // NMI -> A000, reset -> 8000, IRQ -> 9000
    Seq(0xFFFA -> 0x00, 0xFFFB -> 0xA0, 0xFFFC -> 0x00, 0xFFFD -> 0x80, 0xFFFE -> 0x00, 0xFFFF -> 0x90).foreach { case (a, v) => mem(a) = v }
    val log = ArrayBuffer[(Access, Boolean)]()
    compiled.doSim { dut =>
      val cd = dut.clockDomain
      val io = dut.host[BusPlugin].logic.io
      val int = dut.host[Ricoh2A03.InterruptPlugin].logic
      val trace = dut.host[TracePlugin].logic
      io.stall #= false
      int.nmi #= false
      int.irq #= false
      cd.forkStimulus(10)
      cd.waitSampling()
      for (c <- 0 until cycles) {
        int.irq #= irqAt(c)
        int.nmi #= nmiAt(c)
        sleep(1)
        val a = io.cmd.address.toInt
        val acc =
          if (io.cmd.write.toBoolean) { mem(a) = io.cmd.data.toInt; Access(true, a, mem(a)) }
          else Access(false, a, mem(a))
        if (!acc.write) io.rdata #= acc.data
        log += acc -> trace.fetch.toBoolean
        cd.waitSampling()
      }
    }
    log.toSeq
  }
  def fetches(log: Seq[(Access, Boolean)]) = log.filter(_._2).map(_._1.address)

  test("IRQ waits one instruction after CLI, then dispatches with B clear") {
    // 8000 LDA #0; CLI; NOP; NOP; JMP 8004.  9000: JMP 9000
    val log = run(Map(0x8000 -> Seq(0xA9, 0x00, 0x58, 0xEA, 0xEA, 0x4C, 0x04, 0x80), 0x9000 -> Seq(0x4C, 0x00, 0x90)), 40, irqAt = _ => true)
    val f = fetches(log)
    assert(f.take(5) == Seq(0x8000, 0x8002, 0x8003, 0x8004, 0x9000), f.map(a => f"$a%04X").mkString(" "))
    // The fetch at 8004 is the one taken over: read 8004 again, push 8004 and P, read the vector.
    val at = log.indexWhere(e => e._2 && e._1.address == 0x8004)
    val expected = Seq(
      Access(false, 0x8004, 0xEA),
      Access(false, 0x8004, 0xEA),
      Access(true, 0x01FD, 0x80),
      Access(true, 0x01FC, 0x04),
      Access(true, 0x01FB, 0x22),
      Access(false, 0xFFFE, 0x00),
      Access(false, 0xFFFF, 0x90),
      Access(false, 0x9000, 0x4C)
    )
    assert(log.slice(at, at + 8).map(_._1) == expected, log.slice(at, at + 8).map(_._1).mkString(", "))
  }

  test("NMI is taken once per rising edge, I flag or not") {
    // 8000: JMP 8000.  A000: JMP A000
    val log = run(Map(0x8000 -> Seq(0x4C, 0x00, 0x80), 0xA000 -> Seq(0x4C, 0x00, 0xA0)), 60, nmiAt = c => c >= 20)
    val f = fetches(log)
    assert(f.count(_ == 0xA000) > 3 && f.dropWhile(_ != 0xA000).forall(_ == 0xA000), f.map(a => f"$a%04X").mkString(" "))
    val pushedP = log.collect { case (Access(true, 0x01FB, v), _) => v }
    assert(pushedP == Seq(0x24), s"pushed P: $pushedP")
  }

  test("a taken branch that stays on its page lets one more instruction through") {
    // 8000 CLI; LDX #0; BEQ +0 (taken, same page); NOP; JMP 8005
    val program = Map(0x8000 -> Seq(0x58, 0xA2, 0x00, 0xF0, 0x00, 0xEA, 0x4C, 0x05, 0x80), 0x9000 -> Seq(0x4C, 0x00, 0x90))
    // BEQ is fetched at cycle 11 (reset 7, CLI 2, LDX 2); its operand read is 12 and its dummy
    // read 13. IRQ rises in cycle 13, so only the NOP's poll sees it.
    val log = run(program, 40, irqAt = _ >= 13)
    val f = fetches(log)
    assert(f.take(6) == Seq(0x8000, 0x8001, 0x8003, 0x8005, 0x8006, 0x9000), f.map(a => f"$a%04X").mkString(" "))
  }
}
