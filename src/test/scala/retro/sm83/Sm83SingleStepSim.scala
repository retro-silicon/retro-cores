package retro.sm83

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import spinal.core.sim._

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** SingleStepTests/sm83 (MIT, https://github.com/SingleStepTests/sm83): 1000 tests per opcode,
  * each an initial CPU and RAM state, the final state, and the bus state of every M-cycle from
  * the opcode fetch to the end of the instruction (the overlapped fetch of the next opcode is
  * not listed). `scripts/fetch-sm83-tests.sh` downloads them to `roms/sst-sm83`; without them the
  * suite is skipped. Both the RTL and `Sm83Reference` are checked, M-cycle by M-cycle: idle,
  * read or write, and address and data on reads and writes. The address the tests list on idle
  * cycles is whatever was last on the pins, so it is not compared.
  *
  * HALT and STOP are reported but not required to pass: the tests come from ares, which runs
  * HALT as two idle cycles and STOP as three, while here HALT keeps fetching at PC (the Mooneye
  * timing rules) and STOP is a NOP. `SST_OPS` (comma separated, e.g. `f1,cb 46`) and `SST_LIMIT`
  * (tests per opcode) narrow a run. */
class Sm83SingleStepSim extends AnyFunSuite {
  val dir = new File("roms/sst-sm83/v1")
  val excluded = Set(0x76, 0x10)

  /** One M-cycle as the tests list it: kind '-' idle, 'r' read, 'w' write. */
  case class Cycle(kind: Char, address: Int, data: Int) {
    override def toString = if (kind == '-') "idle" else f"$kind $address%04X $data%02X"
    def matches(o: Cycle) = kind == o.kind && (kind == '-' || (address == o.address && data == o.data))
  }
  case class State(regs: Map[String, Int], ram: Seq[(Int, Int)])
  case class Test(name: String, initial: State, expected: State, cycles: Seq[Cycle])
  /** What a model did: the cycles before the next opcode fetch, where that fetch was, and after it. */
  case class Outcome(cycles: Seq[Cycle], nextPc: Int, regs: Map[String, Int], ram: Int => Int)

  val regNames = Seq("a", "f", "b", "c", "d", "e", "h", "l", "sp", "ime", "ei")

  def load(file: File): Seq[Test] = {
    def state(o: ujson.Value) = State(
      regNames.map(n => n -> o.obj.get(n).map(_.num.toInt).getOrElse(0)).toMap + ("pc" -> o("pc").num.toInt),
      o("ram").arr.map(e => e(0).num.toInt -> e(1).num.toInt).toSeq)
    ujson.read(file).arr.map { t =>
      Test(t("name").str, state(t("initial")), state(t("final")), t("cycles").arr.map { c =>
        val pins = c(2).str
        val kind = if (pins.contains('r')) 'r' else if (pins.contains('w')) 'w' else '-'
        Cycle(kind, c(0).num.toInt, if (c(1).isNull) 0 else c(1).num.toInt)
      }.toSeq)
    }.toSeq
  }

  def files: Seq[(Int, File)] = {
    val only = sys.env.get("SST_OPS").map(_.split(",").map(_.trim.toLowerCase).toSet)
    dir.listFiles().filter(_.getName.endsWith(".json")).map { f =>
      val n = f.getName.stripSuffix(".json")
      val opcode = if (n.startsWith("cb ")) Sm83.CB_PAGE | Integer.parseInt(n.drop(3), 16) else Integer.parseInt(n, 16)
      (opcode, f, n)
    }.filter(t => only.forall(_.contains(t._3))).sortBy(_._1).map(t => (t._1, t._2)).toSeq
  }
  val limit = sys.env.get("SST_LIMIT").map(_.toInt).getOrElse(Int.MaxValue)

  def check(t: Test, o: Outcome): Seq[String] = {
    val errors = ArrayBuffer[String]()
    val got = o.cycles
    if (got.size != t.cycles.size || !got.zip(t.cycles).forall { case (g, e) => g.matches(e) })
      errors += s"cycles: got ${got.mkString(", ")}; expected ${t.cycles.mkString(", ")}"
    if (o.nextPc != t.expected.regs("pc")) errors += f"PC ${o.nextPc}%04X, expected ${t.expected.regs("pc")}%04X"
    for (n <- regNames if o.regs(n) != t.expected.regs(n)) errors += f"$n ${o.regs(n)}%02X, expected ${t.expected.regs(n)}%02X"
    for ((a, v) <- t.expected.ram if o.ram(a) != v) errors += f"($a%04X) ${o.ram(a)}%02X, expected $v%02X"
    errors.toSeq
  }

  /** Runs every test through `run` and writes a per-opcode report to out/. */
  def suite(label: String)(run: Seq[Test] => Seq[Outcome]): Unit = {
    assume(dir.isDirectory, s"$dir missing: run scripts/fetch-sm83-tests.sh")
    new File("out").mkdirs()
    val report = new PrintWriter(new File(s"out/sm83-sst-$label.txt"))
    var total, passed = 0
    val failing = ArrayBuffer[String]()
    for ((opcode, file) <- files) {
      val tests = load(file).take(limit)
      val bad = tests.zip(run(tests)).map { case (t, o) => (t, check(t, o)) }.filter(_._2.nonEmpty)
      total += tests.size; passed += tests.size - bad.size
      val name = f"${if (opcode >= Sm83.CB_PAGE) f"CB ${opcode & 0xFF}%02X" else f"$opcode%02X"} ${Sm83.names.getOrElse(opcode, "?")}"
      if (bad.nonEmpty) {
        report.println(f"$name%-16s ${tests.size - bad.size}%4d/${tests.size}  first: ${bad.head._1.name}: ${bad.head._2.mkString("; ")}")
        if (!excluded(opcode)) failing += s"$name (${bad.size} failing)"
      }
    }
    val summary = s"$label: $passed/$total tests pass; ${failing.size} opcodes failing outside HALT/STOP"
    report.println(summary)
    report.close()
    println(summary)
    failing.foreach(f => println(s"  $f"))
    assert(failing.isEmpty, s"failing opcodes: ${failing.mkString(", ")}")
  }

  test("reference model") {
    suite("reference") { tests =>
      tests.map { t =>
        val mem = mutable.HashMap[Int, Int]() ++= t.initial.ram
        val log = ArrayBuffer[Cycle]()
        val bus = new Sm83Bus {
          def read(a: Int) = { val v = mem.getOrElse(a, 0); log += Cycle('r', a, v); v }
          def write(a: Int, v: Int) = { mem(a) = v; log += Cycle('w', a, v) }
          override def idle() = log += Cycle('-', 0, 0)
        }
        val ref = new Sm83Reference(bus, _ => 0, _ => ())
        val r = t.initial.regs
        ref.a = r("a"); ref.f = r("f"); ref.b = r("b"); ref.c = r("c"); ref.d = r("d"); ref.e = r("e")
        ref.h = r("h"); ref.l = r("l"); ref.sp = r("sp"); ref.pc = r("pc"); ref.ime = r("ime") == 1
        ref.reset()
        ref.step()
        val next = log.last
        Outcome(log.init.toSeq, next.address, Map("a" -> ref.a, "f" -> ref.f, "b" -> ref.b, "c" -> ref.c, "d" -> ref.d,
          "e" -> ref.e, "h" -> ref.h, "l" -> ref.l, "sp" -> ref.sp, "ime" -> (if (ref.ime) 1 else 0),
          "ei" -> (if (ref.imeNext) 1 else 0)), a => mem.getOrElse(a, 0))
      }
    }
  }

  test("RTL") {
    lazy val compiled = SimConfig.withVerilator.compile(Sm83Core())
    suite("rtl") { tests =>
      val outcomes = ArrayBuffer[Outcome]()
      compiled.doSim { dut =>
        val cd = dut.clockDomain
        val io = dut.host[BusPlugin].logic.io
        val int = dut.host[Sm83.InterruptPlugin].logic
        val trace = dut.host[TracePlugin].logic
        val rf = dut.host[RegFilePlugin].regs.byReg
        import Sm83Regs._
        io.stall #= false
        int.requests #= 0
        cd.fallingEdge()
        cd.deassertReset()
        sleep(1)
        for (t <- tests) {
          // Reset, then load the state: the CPU sits on the reset NOP's fetch, which becomes the
          // test's opcode fetch.
          cd.assertReset(); sleep(1); cd.deassertReset(); sleep(1)
          val r = t.initial.regs
          for ((reg, n) <- Seq(A -> "a", F -> "f", B -> "b", C -> "c", D -> "d", E -> "e", H -> "h", L -> "l")) rf(reg) #= r(n)
          rf(SPH) #= r("sp") >> 8; rf(SPL) #= r("sp") & 0xFF; rf(PC) #= r("pc")
          int.ime #= r("ime") == 1; int.imeNext #= false
          sleep(1)
          val mem = mutable.HashMap[Int, Int]() ++= t.initial.ram
          val log = ArrayBuffer[Cycle]()
          var nextPc = -1
          var i = 0
          while (nextPc < 0 && i < 16) {
            val c = if (!io.cmd.valid.toBoolean) Cycle('-', 0, 0) else {
              val a = io.cmd.address.toInt
              if (io.cmd.write.toBoolean) { mem(a) = io.cmd.data.toInt; Cycle('w', a, io.cmd.data.toInt) }
              else Cycle('r', a, mem.getOrElse(a, 0))
            }
            if (c.kind == 'r') io.rdata #= c.data
            if (i > 0 && trace.fetch.toBoolean) nextPc = c.address else log += c
            sleep(1); cd.risingEdge(); sleep(1); cd.fallingEdge(); sleep(1)
            i += 1
          }
          val regs = Map("a" -> A, "f" -> F, "b" -> B, "c" -> C, "d" -> D, "e" -> E, "h" -> H, "l" -> L).map { case (n, reg) => n -> rf(reg).toInt } ++
            Map("sp" -> (rf(SPH).toInt << 8 | rf(SPL).toInt), "ime" -> (if (int.ime.toBoolean) 1 else 0), "ei" -> (if (int.imeNext.toBoolean) 1 else 0))
          outcomes += Outcome(log.toSeq, nextPc, regs, a => mem.getOrElse(a, 0))
        }
      }
      outcomes.toSeq
    }
  }
}
