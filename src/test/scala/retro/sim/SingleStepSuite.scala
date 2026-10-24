package retro.sim

import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer

/** A SingleStepTests set (https://github.com/SingleStepTests) for one CPU: per opcode a JSON file
  * of tests, each an initial CPU and RAM state, the final state, and the bus state of every cycle
  * from the opcode fetch to the end of the instruction (the fetch of the next opcode is not
  * listed). A model's outcome is checked cycle by cycle, then registers (`pc` is where it fetches
  * next) and RAM. `SST_OPS` (comma separated file names, e.g. `f1,cb 46`) and `SST_LIMIT` (tests
  * per opcode) narrow or widen a run; per-opcode reports go to `out/<name>-sst-<label>.txt`. */
abstract class SingleStepSuite extends AnyFunSuite {
  def name: String
  def dir: File
  def fetchScript: String
  def regNames: Seq[String]
  def defaultLimit: Int
  /** The opcode a test file covers, from its name without `.json`. */
  def opcode(file: String): Int
  def opcodeName(opcode: Int): String
  /** Opcodes reported but not required to pass. */
  def excluded: Set[Int] = Set()

  /** One cycle as the tests list it: kind '-' idle, 'r' read, 'w' write. An idle cycle's address
    * is whatever was last on the pins, so it is not compared. */
  case class Cycle(kind: Char, address: Int, data: Int) {
    override def toString = if (kind == '-') "idle" else f"$kind $address%04X $data%02X"
    def matches(o: Cycle) = kind == o.kind && (kind == '-' || (address == o.address && data == o.data))
  }
  case class State(regs: Map[String, Int], ram: Seq[(Int, Int)])
  case class Test(name: String, initial: State, expected: State, cycles: Seq[Cycle])
  case class Outcome(cycles: Seq[Cycle], regs: Map[String, Int], ram: Int => Int)

  def cycle(c: ujson.Value): Cycle
  val limit = sys.env.get("SST_LIMIT").map(_.toInt).getOrElse(defaultLimit)

  def load(file: File): Seq[Test] = {
    def state(o: ujson.Value) = State(regNames.map(n => n -> o.obj.get(n).map(_.num.toInt).getOrElse(0)).toMap,
      o("ram").arr.map(e => e(0).num.toInt -> e(1).num.toInt).toSeq)
    ujson.read(file).arr.iterator.take(limit).map { t =>
      Test(t("name").str, state(t("initial")), state(t("final")), t("cycles").arr.map(cycle).toSeq)
    }.toSeq
  }

  def files: Seq[(Int, File)] = {
    val only = sys.env.get("SST_OPS").map(_.split(",").map(_.trim.toLowerCase).toSet)
    dir.listFiles().filter(_.getName.endsWith(".json")).map(_.getName.stripSuffix(".json"))
      .filter(n => only.forall(_.contains(n))).map(n => opcode(n) -> new File(dir, s"$n.json")).sortBy(_._1).toSeq
  }

  def check(t: Test, o: Outcome): Seq[String] = {
    val errors = ArrayBuffer[String]()
    if (o.cycles.size != t.cycles.size || !o.cycles.zip(t.cycles).forall { case (g, e) => g.matches(e) })
      errors += s"cycles: got ${o.cycles.mkString(", ")}; expected ${t.cycles.mkString(", ")}"
    for (n <- regNames if o.regs(n) != t.expected.regs(n)) errors += f"$n ${o.regs(n)}%02X, expected ${t.expected.regs(n)}%02X"
    for ((a, v) <- t.expected.ram if o.ram(a) != v) errors += f"($a%04X) ${o.ram(a)}%02X, expected $v%02X"
    errors.toSeq
  }

  /** Checks every opcode file. `session` sets up a model once and hands the body a function that
    * turns a batch of tests into outcomes. */
  def checkAll(label: String)(session: ((Seq[Test] => Seq[Outcome]) => Unit) => Unit): Unit = {
    assume(dir.isDirectory, s"$dir missing: run $fetchScript")
    new File("out").mkdirs()
    val report = new PrintWriter(new File(s"out/$name-sst-$label.txt"))
    var total, passed = 0
    val failing = ArrayBuffer[String]()
    session { run =>
      for ((op, file) <- files) {
        val tests = load(file)
        val bad = tests.zip(run(tests)).map { case (t, o) => (t, check(t, o)) }.filter(_._2.nonEmpty)
        total += tests.size; passed += tests.size - bad.size
        if (bad.nonEmpty) {
          report.println(f"${opcodeName(op)}%-16s ${tests.size - bad.size}%5d/${tests.size}  first: ${bad.head._1.name}: ${bad.head._2.mkString("; ")}")
          if (!excluded(op)) failing += s"${opcodeName(op)} (${bad.size} failing)"
        }
      }
    }
    val summary = s"$label: $passed/$total tests pass; ${failing.size} opcodes failing" + (if (excluded.isEmpty) "" else " outside the excluded ones")
    report.println(summary)
    report.close()
    println(summary)
    failing.foreach(f => println(s"  $f"))
    assert(failing.isEmpty, s"failing opcodes: ${failing.mkString(", ")}")
  }
}
