package retro.r2a03

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import spinal.core.sim._

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** SingleStepTests/65x02 `nes6502` (MIT, https://github.com/SingleStepTests/65x02): 10,000 tests
  * per opcode, all 256 included, for the 2A03's 6502 core without decimal mode. Each is an initial
  * CPU and RAM state, the final state, and the bus access of every cycle from the opcode fetch to
  * the end of the instruction (the fetch of the next opcode is not listed). Every cycle is
  * compared: read or write, address and data.
  *
  * `scripts/fetch-nes6502-tests.sh` downloads them to `roms/sst-nes6502`; without them the suite
  * is skipped. `SST_OPS` (comma separated, e.g. `6c,b1`) and `SST_LIMIT` (tests per opcode,
  * default 1000) narrow or widen a run. */
class R2A03SingleStepSim extends AnyFunSuite {
  val dir = new File("roms/sst-nes6502")
  val jams = Set(0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2)

  case class Cycle(write: Boolean, address: Int, data: Int) {
    override def toString = f"${if (write) "w" else "r"} $address%04X $data%02X"
  }
  case class State(regs: Map[String, Int], ram: Seq[(Int, Int)])
  case class Test(name: String, initial: State, expected: State, cycles: Seq[Cycle])
  case class Outcome(cycles: Seq[Cycle], regs: Map[String, Int], ram: Int => Int)

  val regNames = Seq("pc", "s", "a", "x", "y", "p")

  def load(file: File): Seq[Test] = {
    def state(o: ujson.Value) = State(regNames.map(n => n -> o(n).num.toInt).toMap,
      o("ram").arr.map(e => e(0).num.toInt -> e(1).num.toInt).toSeq)
    ujson.read(file).arr.iterator.take(limit).map { t =>
      Test(t("name").str, state(t("initial")), state(t("final")),
        t("cycles").arr.map(c => Cycle(c(2).str == "write", c(0).num.toInt, c(1).num.toInt)).toSeq)
    }.toSeq
  }

  def files: Seq[(Int, File)] = {
    val only = sys.env.get("SST_OPS").map(_.split(",").map(_.trim.toLowerCase).toSet)
    dir.listFiles().filter(_.getName.endsWith(".json")).map(f => f.getName.stripSuffix(".json"))
      .filter(n => only.forall(_.contains(n))).sorted.map(n => Integer.parseInt(n, 16) -> new File(dir, s"$n.json")).toSeq
  }
  val limit = sys.env.get("SST_LIMIT").map(_.toInt).getOrElse(1000)

  def check(t: Test, o: Outcome): Seq[String] = {
    val errors = ArrayBuffer[String]()
    if (o.cycles != t.cycles) errors += s"cycles: got ${o.cycles.mkString(", ")}; expected ${t.cycles.mkString(", ")}"
    for (n <- regNames if o.regs(n) != t.expected.regs(n)) errors += f"$n ${o.regs(n)}%02X, expected ${t.expected.regs(n)}%02X"
    for ((a, v) <- t.expected.ram if o.ram(a) != v) errors += f"($a%04X) ${o.ram(a)}%02X, expected $v%02X"
    errors.toSeq
  }

  test("RTL") {
    assume(dir.isDirectory, s"$dir missing: run scripts/fetch-nes6502-tests.sh")
    new File("out").mkdirs()
    val report = new PrintWriter(new File("out/nes6502-sst-rtl.txt"))
    var total, passed = 0
    val failing = ArrayBuffer[String]()
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

      def run(t: Test): Outcome = {
        // Reset, let the reset sequence run up to its fetch at the vector, then load the state:
        // that fetch becomes the test's opcode fetch.
        cd.assertReset(); sleep(1); cd.deassertReset(); sleep(1)
        io.rdata #= 0
        var n = 0
        while (!trace.fetch.toBoolean) { clock(); n += 1; assert(n < 10, "reset sequence does not reach a fetch") }
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
          val c = if (io.cmd.write.toBoolean) { mem(a) = io.cmd.data.toInt; Cycle(true, a, io.cmd.data.toInt) }
                  else Cycle(false, a, mem.getOrElse(a, 0))
          if (!c.write) io.rdata #= c.data
          if (i > 0 && trace.fetch.toBoolean) nextPc = a else log += c
          clock()
          i += 1
        }
        val pc = if (jam) rf(PCH).toInt << 8 | rf(PCL).toInt else nextPc
        val regs = Map("pc" -> pc, "s" -> rf(SP).toInt, "a" -> rf(A).toInt, "x" -> rf(X).toInt, "y" -> rf(Y).toInt, "p" -> rf(P).toInt)
        Outcome(log.toSeq, regs, a => mem.getOrElse(a, 0))
      }

      for ((opcode, file) <- files) {
        val tests = load(file)
        val bad = tests.map(t => (t, check(t, run(t)))).filter(_._2.nonEmpty)
        total += tests.size; passed += tests.size - bad.size
        val name = f"$opcode%02X ${Ricoh2A03.names.getOrElse(opcode, "?")}"
        if (bad.nonEmpty) {
          report.println(f"$name%-14s ${tests.size - bad.size}%5d/${tests.size}  first: ${bad.head._1.name}: ${bad.head._2.mkString("; ")}")
          failing += s"$name (${bad.size} failing)"
        }
      }
    }
    val summary = s"rtl: $passed/$total tests pass; ${failing.size} opcodes failing"
    report.println(summary)
    report.close()
    println(summary)
    failing.foreach(f => println(s"  $f"))
    assert(failing.isEmpty, s"failing opcodes: ${failing.mkString(", ")}")
  }
}
