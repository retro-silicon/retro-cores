package retro.r2a03

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import spinal.core.sim._

import java.io.File
import java.nio.file.Files
import scala.io.Source

/** nestest (Kevin Horton) in automation mode: start at C000 and compare every instruction with
  * the log from Nintendulator, `nestest.log`: the address, A, X, Y, P, S before it, and the cycle
  * count at its opcode fetch. The ROM covers every official opcode and the common undocumented
  * ones; at the end it leaves its error codes in 02 and 03 (00 means passed).
  *
  * The bus is a minimal NES: 2 KB RAM mirrored to 1FFF and the 16 KB PRG ROM mirrored at 8000 and
  * C000; everything else reads FF. `scripts/fetch-nes-test-roms.sh` downloads the ROM and the log
  * to `roms/nes`; without them the test is skipped. */
class R2A03NestestSim extends AnyFunSuite {
  val dir = new File("roms/nes")
  val Line = """^([0-9A-F]{4}) .*A:([0-9A-F]{2}) X:([0-9A-F]{2}) Y:([0-9A-F]{2}) P:([0-9A-F]{2}) SP:([0-9A-F]{2}) .*CYC:(\d+)$""".r
  case class Step(pc: Int, a: Int, x: Int, y: Int, p: Int, s: Int, cycle: Long) {
    override def toString = f"$pc%04X A:$a%02X X:$x%02X Y:$y%02X P:$p%02X SP:$s%02X CYC:$cycle"
  }

  test("nestest matches the Nintendulator log") {
    val rom = new File(dir, "nestest.nes")
    assume(rom.isFile, s"$rom missing: run scripts/fetch-nes-test-roms.sh")
    val image = Files.readAllBytes(rom.toPath).map(_ & 0xFF)
    val prg = image.slice(16, 16 + 0x4000)
    val log = Source
      .fromFile(new File(dir, "nestest.log"))
      .getLines()
      .map { case Line(pc, a, x, y, p, s, cyc) =>
        Step(
          Integer.parseInt(pc, 16),
          Integer.parseInt(a, 16),
          Integer.parseInt(x, 16),
          Integer.parseInt(y, 16),
          Integer.parseInt(p, 16),
          Integer.parseInt(s, 16),
          cyc.toLong
        )
      }
      .toIndexedSeq

    SimConfig.withVerilator.compile(R2A03Core()).doSim { dut =>
      val cd = dut.clockDomain
      val io = dut.host[BusPlugin].logic.io
      val int = dut.host[Ricoh2A03.InterruptPlugin].logic
      val trace = dut.host[TracePlugin].logic
      val rf = dut.host[RegFilePlugin].regs.byReg
      import R2A03Regs._
      val ram = Array.fill(0x800)(0)
      def read(a: Int) = if (a < 0x2000) ram(a & 0x7FF) else if (a >= 0x8000) prg(a & 0x3FFF) else 0xFF
      io.stall #= false
      int.nmi #= false
      int.irq #= false
      cd.forkStimulus(10)
      cd.waitSampling()

      // Each cycle: answer the bus combinationally, then clock. The reset sequence takes 7
      // cycles, so its fetch is cycle 7 as in the log; PC, S and P are set there.
      var cycle = 0L
      var index = 0
      var pendingFetch: Option[(Int, Long)] = None
      var started = false
      while (index < log.size) {
        sleep(1)
        if (trace.fetch.toBoolean && !started) {
          rf(PCH) #= 0xC0; rf(PCL) #= 0x00; rf(SP) #= 0xFD; rf(P) #= 0x24
          started = true
          cycle = 7
          sleep(1)
        }
        // Registers are final one cycle after a fetch: the previous instruction writes back during it.
        for ((pc, at) <- pendingFetch) {
          val got = Step(pc, rf(A).toInt, rf(X).toInt, rf(Y).toInt, rf(P).toInt, rf(SP).toInt, at)
          assert(got == log(index), s"instruction ${index + 1} of ${log.size}\n  rtl ${got}\n  log ${log(index)}")
          index += 1
          pendingFetch = None
        }
        val a = io.cmd.address.toInt
        if (io.cmd.write.toBoolean) { if (a < 0x2000) ram(a & 0x7FF) = io.cmd.data.toInt }
        else io.rdata #= read(a)
        if (started && trace.fetch.toBoolean) pendingFetch = Some((a, cycle))
        cd.waitSampling()
        cycle += 1
        assert(cycle < 100000, "runaway")
      }
      println(f"nestest: ${log.size} instructions match the log; result codes 02=${ram(2)}%02X 03=${ram(3)}%02X")
      assert(ram(2) == 0 && ram(3) == 0, f"nestest reports failure: 02=${ram(2)}%02X 03=${ram(3)}%02X")
    }
  }
}
