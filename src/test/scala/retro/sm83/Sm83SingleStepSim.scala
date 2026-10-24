package retro.sm83

import retro.core._
import retro.sim.SingleStepSuite
import spinal.core.sim._

import java.io.File
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
class Sm83SingleStepSim extends SingleStepSuite {
  def name = "sm83"
  def dir = new File("roms/sst-sm83/v1")
  def fetchScript = "scripts/fetch-sm83-tests.sh"
  def regNames = Seq("pc", "a", "f", "b", "c", "d", "e", "h", "l", "sp", "ime", "ei")
  def defaultLimit = Int.MaxValue
  def opcode(file: String) = if (file.startsWith("cb ")) Sm83.CB_PAGE | Integer.parseInt(file.drop(3), 16) else Integer.parseInt(file, 16)
  def opcodeName(opcode: Int) = f"${if (opcode >= Sm83.CB_PAGE) f"CB ${opcode & 0xFF}%02X" else f"$opcode%02X"} ${Sm83.names.getOrElse(opcode, "?")}"
  override def excluded = Set(0x76, 0x10)
  def cycle(c: ujson.Value) = {
    val pins = c(2).str
    Cycle(if (pins.contains('r')) 'r' else if (pins.contains('w')) 'w' else '-', c(0).num.toInt, if (c(1).isNull) 0 else c(1).num.toInt)
  }

  test("reference model") {
    checkAll("reference")(body => body(_.map { t =>
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
        Outcome(log.init.toSeq, Map("pc" -> log.last.address, "a" -> ref.a, "f" -> ref.f, "b" -> ref.b, "c" -> ref.c, "d" -> ref.d,
          "e" -> ref.e, "h" -> ref.h, "l" -> ref.l, "sp" -> ref.sp, "ime" -> (if (ref.ime) 1 else 0),
          "ei" -> (if (ref.imeNext) 1 else 0)), a => mem.getOrElse(a, 0))
    }))
  }

  test("RTL") {
    checkAll("rtl") { body =>
      SimConfig.withVerilator.compile(Sm83Core()).doSim { dut =>
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
        def runOne(t: Test): Outcome = {
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
            Map("pc" -> nextPc, "sp" -> (rf(SPH).toInt << 8 | rf(SPL).toInt), "ime" -> (if (int.ime.toBoolean) 1 else 0), "ei" -> (if (int.imeNext.toBoolean) 1 else 0))
          Outcome(log.toSeq, regs, a => mem.getOrElse(a, 0))
        }
        body(_.map(runOne))
      }
    }
  }
}
