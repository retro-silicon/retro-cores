package retro.r2a03

import retro.core._
import retro.sim.SingleStepSuite
import spinal.core.sim._

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** SingleStepTests/65x02 `nes6502` (MIT, https://github.com/SingleStepTests/65x02): 10,000 tests
  * per opcode, all 256 included, for the 2A03's 6502 core without decimal mode. Each is an initial
  * CPU and RAM state, the final state, and the bus access of every cycle from the opcode fetch to
  * the end of the instruction (the fetch of the next opcode is not listed). Both the RTL and
  * `R2A03Reference` are checked, every cycle: read or write, address and data.
  *
  * `scripts/fetch-nes6502-tests.sh` downloads them to `roms/sst-nes6502`; without them the suite
  * is skipped. `SST_OPS` (comma separated, e.g. `6c,b1`) and `SST_LIMIT` (tests per opcode,
  * default 1000) narrow or widen a run. */
class R2A03SingleStepSim extends SingleStepSuite {
  def name = "nes6502"
  def dir = new File("roms/sst-nes6502")
  def fetchScript = "scripts/fetch-nes6502-tests.sh"
  def regNames = Seq("pc", "s", "a", "x", "y", "p")
  def defaultLimit = 1000
  def opcode(file: String) = Integer.parseInt(file, 16)
  def opcodeName(opcode: Int) = f"$opcode%02X ${Ricoh2A03.names.getOrElse(opcode, "?")}"
  def cycle(c: ujson.Value) = Cycle(if (c(2).str == "write") 'w' else 'r', c(0).num.toInt, c(1).num.toInt)
  val jams = Ricoh2A03.JAMS

  test("reference model") {
    checkAll("reference")(body =>
      body(_.map { t =>
        val mem = mutable.HashMap[Int, Int]() ++= t.initial.ram
        val log = ArrayBuffer[Cycle]()
        val ref = new R2A03Reference(new R2A03Bus {
          def read(a: Int) = { val v = mem.getOrElse(a, 0); log += Cycle('r', a, v); v }
          def write(a: Int, v: Int) = { mem(a) = v; log += Cycle('w', a, v) }
        })
        val r = t.initial.regs
        ref.a = r("a"); ref.x = r("x"); ref.y = r("y"); ref.s = r("s"); ref.p = r("p"); ref.pc = r("pc")
        ref.fetch()
        val jam = jams(t.initial.ram.find(_._1 == r("pc")).get._2)
        if (jam) { while (log.size < t.cycles.size) ref.step() }
        else ref.step()
        val cycles = if (jam) log.take(t.cycles.size) else log.init
        val pc = if (jam) ref.pc else ref.fetchAddress
        Outcome(cycles.toSeq, Map("pc" -> pc, "s" -> ref.s, "a" -> ref.a, "x" -> ref.x, "y" -> ref.y, "p" -> ref.p), a => mem.getOrElse(a, 0))
      }))
  }

  test("RTL") {
    checkAll("rtl") { body =>
      SimConfig.withVerilator.compile(R2A03Core()).doSim { dut =>
        val cd = dut.clockDomain
        val io = dut.host[BusPlugin].logic.io
        val int = dut.host[Ricoh2A03.InterruptPlugin].logic
        val trace = dut.host[TracePlugin].logic
        val rf = dut.host[RegFilePlugin].regs.byReg
        import R2A03Regs._
        io.stall #= false
        io.rdata #= 0
        int.nmi #= false
        int.irq #= false
        cd.fallingEdge()
        sleep(1)

        def clock() = { sleep(1); cd.risingEdge(); sleep(1); cd.fallingEdge(); sleep(1) }
        // Clock through a reset once so no register starts from the simulator's initial values.
        cd.assertReset(); clock(); clock(); cd.deassertReset()

        def runOne(t: Test): Outcome = {
          // Reset, let the reset sequence run up to its fetch at the vector, then load the state:
          // that fetch becomes the test's opcode fetch.
          cd.assertReset(); sleep(1); cd.deassertReset(); sleep(1)
          io.rdata #= 0
          var n = 0
          while (!trace.fetch.toBoolean) {
            clock(); n += 1; assert(n < 10, s"reset sequence does not reach a fetch: opcode ${trace.opcode.toInt} step ${trace.step.toInt}")
          }
          val r = t.initial.regs
          for ((reg, k) <- Seq(A -> "a", X -> "x", Y -> "y", SP -> "s", P -> "p")) rf(reg) #= r(k)
          rf(PCH) #= r("pc") >> 8; rf(PCL) #= r("pc") & 0xFF
          sleep(1)
          val mem = mutable.HashMap[Int, Int]() ++= t.initial.ram
          val log = ArrayBuffer[Cycle]()
          var nextPc = -1
          var i = 0
          val jam = jams(t.initial.ram.find(_._1 == r("pc")).get._2)
          while (nextPc < 0 && i < 12 && !(jam && i == t.cycles.size)) {
            val a = io.cmd.address.toInt
            val c =
              if (io.cmd.write.toBoolean) { mem(a) = io.cmd.data.toInt; Cycle('w', a, io.cmd.data.toInt) }
              else Cycle('r', a, mem.getOrElse(a, 0))
            if (c.kind == 'r') io.rdata #= c.data
            if (i > 0 && trace.fetch.toBoolean) nextPc = a else log += c
            clock()
            i += 1
          }
          val pc = if (jam) rf(PCH).toInt << 8 | rf(PCL).toInt else nextPc
          val regs = Map("pc" -> pc, "s" -> rf(SP).toInt, "a" -> rf(A).toInt, "x" -> rf(X).toInt, "y" -> rf(Y).toInt, "p" -> rf(P).toInt)
          Outcome(log.toSeq, regs, a => mem.getOrElse(a, 0))
        }
        body(_.map(runOne))
      }
    }
  }
}
