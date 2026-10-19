package retro.gb

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.sm83._
import spinal.core._
import spinal.core.sim._

import java.io.File
import java.nio.file.Files
import scala.collection.mutable

/** Runs hardware test ROMs on the `GameBoy` RTL with a `CartridgePlugin`: Blargg's CPU tests and
  * the Mooneye Test Suite. `scripts/fetch-test-roms.sh` downloads them into `roms/` (or set
  * `GB_TEST_ROMS`); without them the suite is skipped.
  *
  * Blargg ROMs report over serial ("Passed" / "Failed"), and those with cartridge RAM also at
  * A000 (status byte, signature DE B0 61, text). Mooneye ROMs end with `LD B,B` and loop, with
  * B C D E H L = 3 5 8 13 21 34 for a pass and all 0x42 for a failure. (They also send those
  * bytes over serial, but without waiting for a DMG-speed transfer to finish, so the bytes
  * arrive shifted.) */
object GbRomRunner {
  val root = new File(sys.env.getOrElse("GB_TEST_ROMS", "roms"))

  case class Outcome(passed: Boolean, detail: String, mcycles: Long)

  private val compiled = mutable.Map[CartridgeSpec, SimCompiled[RetroCore]]()
  def compile(spec: CartridgeSpec) = compiled.getOrElseUpdate(spec,
    SimConfig.withVerilator.allOptimisation.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(1 MHz)))
      .addSimulatorFlag("-O3").workspacePath(sys.env.getOrElse("SIM_WORKSPACE", "simWorkspace")).workspaceName(f"GameBoyRom_${spec.romBytes}%x_${spec.ramBytes}%x_${spec.mbc}")
      .compile(GameBoy(withTrace = false, cartridge = Some(spec))))

  def run(rom: File, budget: Long): Outcome = {
    val image = Files.readAllBytes(rom.toPath)
    // Blargg's ROMs write their report to cartridge RAM even when the header declares none
    // (halt_bug), as emulators provide; give every MBC cartridge at least one RAM bank.
    val spec = CartridgeSpec.fromHeader(image) match {
      case s if s.mbc != Mbc.None && s.ramBytes < 0x2000 => s.copy(ramBytes = 0x2000)
      case s => s
    }
    var outcome: Outcome = null
    compile(spec).doSimUntilVoid { dut =>
      val cart = dut.host[CartridgePlugin]
      cart.load(image)
      val tx = dut.host[SerialPlugin].tx
      dut.host[JoypadPlugin].buttons #= 0
      val io = dut.host[BusPlugin].logic.io
      io.rdata #= 0xFF
      io.stall #= false
      val serial = new StringBuilder
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings { if (tx.valid.toBoolean) serial += tx.payload.toInt.toChar }

      def ramText(): Option[(Int, String)] = cart.logic.ram match {
        case null => None
        case ram =>
          def at(a: Int) = ram.mem.getBigInt(a).toInt
          if (at(1) != 0xDE || at(2) != 0xB0 || at(3) != 0x61 || at(0) == 0x80) None
          else Some(at(0) -> Iterator.from(4).map(at).takeWhile(_ != 0).map(_.toChar).mkString)
      }
      val rf = dut.host[RegFilePlugin].regs.byReg
      def reg(r: Register) = rf(r).toInt
      import Sm83Regs._
      var last: (Seq[Int], Int) = null
      var cycles = 0L
      val step = 1L << 15
      while (outcome == null) {
        dut.clockDomain.waitSampling(step.toInt)
        cycles += step
        val text = serial.toString
        // Mooneye ends in LD B,B / JR -2: the registers stop changing and PC stays on the JR.
        val now = (Seq(B, C, D, E, H, L).map(reg), reg(PC))
        val stopped = last != null && now._1 == last._1 && (now._2 - last._2).abs <= 2
        last = now
        outcome =
          if (stopped && now._1 == Seq(3, 5, 8, 13, 21, 34)) Outcome(true, "Fibonacci", cycles)
          else if (stopped && now._1.forall(_ == 0x42)) Outcome(false, "Mooneye failure", cycles)
          else if (text.contains("Passed")) Outcome(true, text.trim, cycles)
          else if (text.contains("Failed")) {
            dut.clockDomain.waitSampling(400000) // let the rest of the report arrive
            Outcome(false, serial.toString.trim, cycles)
          } else ramText() match {
            case Some((0, t)) => Outcome(true, t.trim, cycles)
            case Some((code, t)) => Outcome(false, s"code $code: ${t.trim}", cycles)
            case None if cycles >= budget => Outcome(false, s"timeout; serial: ${text.map(c => f"${c.toInt}%02X").mkString(" ").takeRight(60)}", cycles)
            case None => null
          }
      }
      simSuccess()
    }
    outcome
  }

  /** Every runnable ROM under `root`, with its M-cycle budget. */
  def roms: Seq[(String, File, Long)] = {
    def under(dir: String) = {
      val d = new File(root, dir)
      if (!d.isDirectory) Nil
      else Files.walk(d.toPath).toArray.map(_.asInstanceOf[java.nio.file.Path].toFile)
        .filter(f => f.isFile && f.getName.endsWith(".gb")).toSeq.sortBy(_.getPath)
    }
    def name(f: File) = root.toPath.relativize(f.toPath).toString.stripSuffix(".gb")
    val blargg = under("blargg").map(f => (name(f), f, if (f.getName == "cpu_instrs.gb") 120000000L else 40000000L))
    val mooneye = under("mooneye/acceptance").map(f => (name(f), f, 8000000L))
    val mbc = under("mooneye/emulator-only").map(f => (name(f), f, 8000000L))
    blargg ++ mooneye ++ mbc
  }
}

/** Prints a pass/fail table for every ROM (or those whose name contains an argument). */
object GbRomReport extends App {
  val selected = GbRomRunner.roms.filter { case (n, _, _) => args.isEmpty || args.exists(n.contains) }
  val results = for ((name, f, budget) <- selected) yield {
    val t0 = System.nanoTime()
    val o = try GbRomRunner.run(f, budget) catch { case e: Throwable => GbRomRunner.Outcome(false, s"error: ${e.getMessage}", 0) }
    val secs = (System.nanoTime() - t0) / 1e9
    println(f"${if (o.passed) "PASS" else "FAIL"}  $name%-60s ${o.mcycles / 1e6}%7.2fM ${secs}%6.1fs  ${o.detail.replace('\n', ' ').take(120)}")
    name -> o
  }
  println(s"${results.count(_._2.passed)}/${results.size} passed")
}

/** Every ROM `GbRomRunner` finds, as a test. ROMs for other models or for hardware not built
  * yet are registered as ignored, with the reason in the name. The combined `cpu_instrs` and
  * `mem_timing` ROMs are left to `GbRomReport`: their individual parts run here, in a fraction
  * of the time. */
class GameBoyRomSim extends AnyFunSuite {
  import GbRomRunner._

  def skipReason(name: String): Option[String] = name match {
    case n if n.endsWith("cpu_instrs/cpu_instrs") || n.endsWith("mem_timing/mem_timing") => Some("combined ROM")
    // Alone it prints every corrupted OAM dump and its text log grows slower to append to, far
    // past the budget; part 7 of the combined oam_bug ROM checks the same CRC, so that one runs.
    case n if n.endsWith("7-timing_effect") => Some("slow alone; checked as part 7 of the combined ROM")
    case n if Seq("-S", "-dmg0", "-mgb", "-sgb", "-sgb2").exists(n.endsWith) => Some("another Game Boy model")
    case n if n.contains("boot_hwio") => Some("needs the APU registers")
    case n if n.contains("/mbc2/") => Some("MBC2 not modelled")
    case n if n.contains("multicart") => Some("MBC1 multicart wiring not modelled")
    case _ => None
  }

  for ((name, f, budget) <- roms) skipReason(name) match {
    case Some(why) => ignore(s"$name ($why)") {}
    case None => test(name) {
      val o = GbRomRunner.run(f, budget)
      assert(o.passed, o.detail)
    }
  }
  if (roms.isEmpty) test("test ROMs") { cancel(s"no ROMs under $root: run scripts/fetch-test-roms.sh") }
}
