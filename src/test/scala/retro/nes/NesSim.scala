package retro.nes

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.r2a03._
import retro.sim._
import spinal.core._
import spinal.core.sim._

import java.io.File
import scala.collection.mutable

/** The `Nes` RTL cosimulated with `NesReference`: every retired instruction must match in
  * cycles (DMA stalls included), ordered bus accesses (so every PPU, APU and controller register
  * read) and CPU registers, every frame must match pixel for pixel, and the APU's mixed output
  * must match in every cycle. */
object NesRtl {
  private val compiled = mutable.Map[(NesCartSpec, NesTiming), SimCompiled[RetroCore]]()
  def compile(spec: NesCartSpec, timing: NesTiming = NesTiming.ntsc) = synchronized(compiled.getOrElseUpdate(
      (spec, timing),
      FastVerilator(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(1 MHz)))
        .workspaceName(f"Nes_${spec.prgBytes}%x_${spec.chrBytes}%x_${spec.chrRam}_${spec.mapper}_${spec.verticalMirroring}${if (timing.pal) "_pal" else ""}")
        .compile(Nes(spec, romDecoder = true, timing = timing))
    ))

  case class Result(verdict: NesRoms.Verdict, frames: Int, instructions: Long, cycles: Long, ref: NesReference)

  /** Runs `file` until its test reports a result (see `NesRoms.verdict`) or `budget` frames.
    * Throws on the first difference between the RTL and the model. */
  def cosim(file: File, budget: Int): Result = {
    val spec = NesCartSpec.of(NesRoms.cartridge(file))
    var result: Result = null
    val timing = NesRoms.timing(file)
    compile(spec, timing).doSimUntilVoid { dut =>
      dut.host[NesCartridgePlugin].load(NesRoms.cartridge(file))
      val ref = new NesReference(NesRoms.cartridge(file), timing = timing)
      val crcs = NesRoms.expectedCrcs(file)
      dut.host[NesJoypadPlugin].buttons #= 0
      val resetButton = dut.host[NesInterruptsPlugin].logic.get.button
      resetButton #= false
      var pressNow = false
      val button = new NesRoms.ResetButton(() => pressNow = true)
      val ppu = dut.host[NesPpuPlugin]
      val pixelTap = ppu.logic.pixelTap

      var failure: Option[Throwable] = None
      var resetDone = false
      var checked = 0L
      val cosim = new TraceBackend {
        override def onRetire(e: RetireEvent) = try {
          // The model's first step is the reset sequence, which the probe does not report.
          if (!resetDone) { ref.cpu.step(); resetDone = true }
          // A reset the button caused: the model takes it at the same instruction boundary.
          if (e.opcode == Ricoh2A03.RESET) ref.reset()
          val opcode = ref.cpu.opcode
          val (bus, cycles) = ref.cpu.step()
          def ctx =
            f"instruction #${e.index} at ${e.address}%04X ${e.opcode}%03X ${Ricoh2A03.names.getOrElse(e.opcode, "?")} (frame ${ref.ppu.frames}, line ${ref.ppu.line}, dot ${ref.ppu.dot})"
          assert(e.opcode == opcode, f"$ctx: the model runs $opcode%03X")
          assert(e.bus == bus, s"$ctx: bus\n  rtl ${e.bus.mkString(", ")}\n  ref ${bus.mkString(", ")}")
          assert(e.mcycles + e.stalls == cycles, s"$ctx: ${e.mcycles}+${e.stalls} cycles, reference says $cycles")
          val refRegs = ref.cpu.regs
          if (!R2A03Regs.architectural.forall(r => e.regs(r.name).toInt == refRegs(r.name))) {
            val rtl = R2A03Regs.architectural.map(r => r.name -> e.regs(r.name).toInt).toMap
            assert(false, s"$ctx: registers\n  rtl $rtl\n  ref $refRegs")
          }
          checked += 1
        } catch { case t: Throwable => if (failure.isEmpty) failure = Some(t) }
      }

      // Frames: the RTL's from its pixel port, the model's from its PPU; compared in order.
      val frame = Array.fill(256 * 240)(0)
      val rtlFrames, refFrames = mutable.Queue[Array[Int]]()
      var compared = 0
      var verdict: NesRoms.Verdict = NesRoms.Running
      var stable = 0
      var last = -1
      ref.ppu.onFrame = f => {
        refFrames += f.clone()
        val now = ref.ram(0xF8) << 16 | NesRoms.screenText(ref).hashCode & 0xFFFF
        stable = if (now == last) stable + 1 else 0
        last = now
        verdict = NesRoms.verdict(ref, stable, crcs, NesRoms.nmiSync(file), NesRoms.holyMapperel(file))
        button.frame(ref)
      }
      // Audio: one sample per cycle from each side, compared in order.
      val audio = dut.host[NesApuPlugin].audio
      val rtlAudio, refAudio = mutable.Queue[Int]()
      ref.onSample = refAudio += _
      var samples = 0L
      var cycles = 0L
      def sample(): Unit = {
        cycles += 1
        resetButton #= pressNow
        pressNow = false
        rtlAudio += audio.toInt
        while (rtlAudio.nonEmpty && refAudio.nonEmpty) {
          val (a, b) = (rtlAudio.dequeue(), refAudio.dequeue())
          if (a != b && failure.isEmpty) failure = Some(new AssertionError(s"audio sample $samples (frame ${ref.ppu.frames}): rtl $a, ref $b"))
          samples += 1
        }
        while (rtlFrames.nonEmpty && refFrames.nonEmpty) {
          val (a, b) = (rtlFrames.dequeue(), refFrames.dequeue())
          val differ = a.indices.count(i => a(i) != b(i))
          if (differ != 0 && failure.isEmpty) {
            val base = file.getName.stripSuffix(".nes")
            NesRoms.save(a, new File(s"out/nes/${base}_rtl_frame_$compared.png"))
            NesRoms.save(b, new File(s"out/nes/${base}_ref_frame_$compared.png"))
            failure = Some(new AssertionError(s"frame $compared: $differ pixels differ (saved to out/nes/)"))
          }
          compared += 1
        }
      }
      val probe = new Probe(dut.host[TracePlugin], dut.clockDomain, cycleEvents = false, autoSample = false).add(cosim)
      // One sampling callback: a pixel every fast clock; the probe and the checks every CPU cycle,
      // at the clock ending it.
      val cpuEnable = dut.cpuEnable
      dut.clockDomain.onSamplings {
        val p = pixelTap.toInt
        if ((p >> 22) != 0) {
          val (y, x) = ((p >> 14) & 0xFF, (p >> 6) & 0xFF)
          frame(y * 256 + x) = p & 0x3F
          if (y == 239 && x == 255) rtlFrames += frame.clone()
        }
        if (cpuEnable.toBoolean) { probe.sample(); sample() }
      }
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSamplingWhere(failure.nonEmpty || verdict != NesRoms.Running || ref.ppu.frames >= budget)
      for (t <- failure) throw t
      assert(samples > cycles - 10, s"only $samples audio samples compared in $cycles cycles")
      result = Result(verdict, compared, checked, cycles, ref)
      simSuccess()
    }
    result
  }
}

class NesSim extends AnyFunSuite {
  // Short ROMs covering the CPU, PPU registers and timing, rendering, sprites and DMA.
  val quick = Seq(
    "ppu_vbl_nmi/rom_singles/01-vbl_basics",
    "ppu_vbl_nmi/rom_singles/02-vbl_set_time",
    "sprite_hit_tests_2005.10.05/01.basics",
    "sprite_overflow_tests/1.Basics",
    "cpu_interrupts_v2/rom_singles/4-irq_and_dma",
    "blargg_ppu_tests_2005.09.15b/vram_access"
  )
  for (name <- quick) test(s"cosim: $name") {
    val file = new File(NesRoms.dir, s"$name.nes")
    assume(file.isFile, s"$file missing: run scripts/fetch-nes-test-roms.sh")
    val r = NesRtl.cosim(file, 400)
    println(s"$name: ${r.verdict}, ${r.instructions} instructions, ${r.frames} frames compared")
    assert(r.verdict.isInstanceOf[NesRoms.Done] && r.verdict.asInstanceOf[NesRoms.Done].passed, r.verdict.toString)
  }
}

/** Every test ROM (or those whose path contains an argument) cosimulated on the RTL, `JOBS`
  * (default: one per core) at a time; each result prints as it finishes, and goes with the ROM's
  * last screen into the `RomReport` `nes-rtl`:
  * {{{ sbt "Test/runMain retro.nes.NesRomReport sprite_hit" }}} */
object NesRomReport extends App {
  import scala.concurrent._
  import scala.concurrent.duration.Duration
  val budget = sys.env.getOrElse("FRAMES", "2000").toInt
  val jobs = sys.env.get("JOBS").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
  val pool = java.util.concurrent.Executors.newFixedThreadPool(jobs)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
  val roms = NesRoms.all.filter { case (name, _) => args.isEmpty || args.exists(name.contains) }
  val report = new RomReport("nes-rtl")
  // Compile each cartridge configuration once, before the runs share it.
  roms.map { case (_, file) => (NesCartSpec.of(NesRoms.cartridge(file)), NesRoms.timing(file)) }.distinct.foreach { case (s, t) => NesRtl.compile(s, t) }
  val start = System.nanoTime()
  val results = Future.sequence(roms.map { case (name, file) =>
    Future {
      val t = System.nanoTime()
      val rom = name.stripSuffix(".nes")
      // The verdict, then the test's text.
      val (pass, verdict, text) =
        try {
          val r = NesRtl.cosim(file, budget)
          val secs = (System.nanoTime() - t) / 1e9
          NesRoms.save(r.ref.ppu.frame, report.frame(rom))
          r.verdict match {
            case NesRoms.Done(true, _, _) => (true, f"PASS      $name (${r.frames} frames, $secs%.0f s)", "")
            case NesRoms.Done(false, code, text) => (false, f"FAIL $code%-4d $name (${r.frames} frames)", text)
            case NesRoms.Running => (false, f"timeout   $name (${r.frames} frames)", "")
          }
        } catch { case t: Throwable => (false, s"MISMATCH  $name", String.valueOf(t.getMessage)) }
      report.add(rom.takeWhile(_ != '/'), rom, if (pass) "PASS" else "FAIL", s"${verdict.replace(s"$name ", "")} $text")
      synchronized(println((verdict +: text.linesIterator.map("    " + _).toSeq).mkString("\n")))
      pass
    }
  })
  val passed = Await.result(results, Duration.Inf).count(identity)
  pool.shutdown()
  report.close()
  println(f"$passed/${roms.size} pass (${(System.nanoTime() - start) / 1e9}%.0f s, $jobs jobs)")
}
