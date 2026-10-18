package retro.toy

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random

class Toy8Sim extends AnyFunSuite {
  // LD HL,8000 / LD A,FD / loop: INC A / LD (HL),A / INC HL / JR NZ,loop / LD A,(HL) / NOP...
  val program = Seq(0x21, 0x00, 0x80, 0x3E, 0xFD, 0x3C, 0x77, 0x23, 0x20, 0xFB, 0x7E)
  val expected = Seq( // (address, mnemonic, M-cycles)
    (0x0000, "LD HL,nn", 3), (0x0003, "LD A,n", 2),
    (0x0005, "INC A", 1), (0x0006, "LD (HL),A", 2), (0x0007, "INC HL", 2), (0x0008, "JR NZ,e", 3),
    (0x0005, "INC A", 1), (0x0006, "LD (HL),A", 2), (0x0007, "INC HL", 2), (0x0008, "JR NZ,e", 3),
    (0x0005, "INC A", 1), (0x0006, "LD (HL),A", 2), (0x0007, "INC HL", 2), (0x0008, "JR NZ,e", 2),
    (0x000A, "LD A,(HL)", 2), (0x000B, "NOP", 1))

  lazy val compiled = SimConfig.withVerilator.compile(Toy8(steal = true))

  def run(waitStates: Boolean, steal: Boolean, seed: Int) = compiled.doSimUntilVoid(seed = seed) { dut =>
    val io = dut.host[BusPlugin].logic.io
    val stealPin = dut.host[Toy8.StealPlugin].logic.steal
    val mem = Array.fill(0x10000)(0)
    program.zipWithIndex.foreach { case (b, i) => mem(i) = b }
    mem(0x8003) = 0x42

    val rec = new RecordingBackend
    val log = new StringBuilder
    new Probe(dut.host[TracePlugin], dut.clockDomain).add(rec).add(new PrintBackend(Toy8.names, l => log ++= l + "\n"))

    val rnd = new Random(seed)
    io.stall #= false
    stealPin #= false
    io.rdata #= 0
    dut.clockDomain.forkStimulus(10)
    // Bus slave: writes land at the edge; reads are asynchronous, answered from the current address.
    dut.clockDomain.onSamplings {
      if (io.cmd.valid.toBoolean && io.cmd.write.toBoolean && !io.stall.toBoolean) mem(io.cmd.address.toInt) = io.cmd.data.toInt
      io.stall #= waitStates && rnd.nextInt(4) == 0
      stealPin #= steal && rnd.nextInt(5) == 0
    }
    fork {
      while (true) { io.rdata #= mem(io.cmd.address.toInt); sleep(1) }
    }
    dut.clockDomain.waitSamplingWhere(rec.retired.size >= expected.size)
    dut.clockDomain.waitSampling(2)

    val got = rec.retired.take(expected.size).map(r => (r.address, Toy8.names.getOrElse(r.opcode, f"?${r.opcode}%02X"), r.mcycles))
    assert(got == expected, "\n" + log)
    assert(mem.slice(0x8000, 0x8003).toSeq == Seq(0xFE, 0xFF, 0x00))
    val last = rec.retired(expected.size - 2) // LD A,(HL)
    assert(last.regs("A") == 0x42 && last.regs("H") == 0x80 && last.regs("L") == 0x03)
    assert(rec.retired(2).regs("F") == 0, "INC A to FE must clear ZF")
    assert(rec.retired(12).regs("F") == 0x80, "INC A to 00 must set ZF")
    if (!waitStates && !steal) assert(rec.retired.take(expected.size).forall(_.stalls == 0))
    else assert(rec.retired.exists(_.stalls > 0))
    simSuccess()
  }

  test("opcode timings, pairs, flags and branches") { run(waitStates = false, steal = false, seed = 1) }
  test("slave wait states stretch cycles without changing results") { run(waitStates = true, steal = false, seed = 2) }
  test("a higher-priority bus master stalls the CPU") { run(waitStates = false, steal = true, seed = 3) }
}
