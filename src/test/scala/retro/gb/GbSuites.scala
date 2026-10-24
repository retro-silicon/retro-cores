package retro.gb

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.sim.{FastVerilator, RomReport}
import retro.sm83._
import spinal.core._
import spinal.core.sim._

import java.io.File
import java.nio.file.Files
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** The test ROMs `scripts/fetch-test-roms.sh` takes from c-sp/game-boy-test-roms into
  * `roms/c-sp` (or `$GB_TEST_ROMS/c-sp`), as a DMG runs them, and how each one reports its result:
  *  - Mealybug Tearoom Tests, dmg-acid2 and AGE's screenshot tests: the frame when `LD B,B`
  *    executes must match the DMG reference screenshot;
  *  - AGE, SameSuite, the Mooneye Test Suite and wilbertpol's fork of it: B C D E H L =
  *    3 5 8 13 21 34 when `LD B,B` (the fork's old `ED`) executes;
  *  - Gambatte: after 15 frames' time, the hex digits in the ROM's name drawn as tiles at the
  *    top left (`_dmg08_out<hex>`), or the frame matching `<name>_dmg08.png`;
  *  - Gambatte's audio tests (`_outaudio0/1`): the output stays constant, or changes, in the
  *    last frame's time;
  *  - GBMicrotest: FF82 = 01 for a pass, FF for a failure;
  *  - blargg's: "Passed" over serial, or A000 = 00 under the DE B0 61 signature;
  *  - Scribbltests, Bully, Strikethrough, firstwhite, TurtleTests, MBC3 Tester: the frame
  *    matching the DMG screenshot after a fixed time.
  * ROMs for other models, or needing hardware not built (APU, MBC2/3, button presses) are listed
  * with the reason they are skipped. */
object GbSuites {
  val root = new File(sys.env.getOrElse("GB_TEST_ROMS", "roms"), "c-sp")
  val frameCycles = Gb.lineCycles * Gb.frameLines
  val LdBB = 0x40

  sealed trait Check
  case class Fibonacci(opcode: Int) extends Check
  /** The frame must match one of `pngs`: when `opcode` executes, or else at any frame end. */
  case class Screenshot(pngs: Seq[File], opcode: Option[Int]) extends Check
  case class HexOut(digits: String) extends Check
  case object Micro extends Check
  /** Gambatte's audio tests: in the last frame's time the output must change (`sound`) or not. */
  case class Audio(sound: Boolean) extends Check
  /** Blargg's: "Passed" or "Failed" over serial, or A001-A003 = DE B0 61 and A000 the result,
    * 00 for a pass. */
  case object Blargg extends Check

  case class Rom(suite: String, name: String, file: File, check: Check, mcycles: Long, skip: Option[String] = None)

  private def files(dir: String, ext: String*): Seq[File] = {
    val d = new File(root, dir)
    if (!d.isDirectory) Nil
    else
      Files
        .walk(d.toPath)
        .iterator()
        .asScala
        .map(_.toFile)
        .filter(f => f.isFile && ext.exists(f.getName.endsWith))
        .toSeq
        .sortBy(_.getPath)
  }
  private def base(f: File) = f.getName.take(f.getName.lastIndexOf('.'))
  private def sibling(f: File, name: String) = Some(new File(f.getParentFile, name)).filter(_.isFile)
  private def name(f: File) = root.toPath.relativize(f.toPath).toString.take(root.toPath.relativize(f.toPath).toString.lastIndexOf('.'))

  def roms: Seq[Rom] = blargg ++ mooneye ++ acid ++ mealybug ++ age ++ gambatte ++ micro ++ wilbertpol ++ same ++ screenshots

  /** Blargg's DMG tests; the combined ROMs repeat or run long, so they get their own time. */
  def blargg = files("blargg", ".gb").map { f =>
    val skip =
      if (Seq("cgb_sound", "interrupt_time").exists(f.getPath.contains)) Some("CGB test")
      // Alone it prints every corrupted OAM dump and its text log grows slower to append to, far
      // past the budget; part 7 of the combined oam_bug ROM checks the same CRC.
      else if (base(f) == "7-timing_effect") Some("slow alone; checked as part 7 of oam_bug")
      else None
    val seconds = f.getName match { case "cpu_instrs.gb" => 60; case "dmg_sound.gb" => 36; case _ => 30 }
    Rom("blargg", name(f), f, Blargg, seconds * 60L * frameCycles, skip)
  }

  def acid = files("dmg-acid2", ".gb").map { f =>
    Rom("dmg-acid2", name(f), f, Screenshot(sibling(f, "dmg-acid2-dmg.png").toSeq, Some(LdBB)), 60 * frameCycles)
  }

  def mealybug = files("mealybug-tearoom-tests", ".gb").map { f =>
    val pngs = Seq("_dmg_blob.png", "_dmg_b.png").flatMap(s => sibling(f, base(f) + s))
    val skip = if (!f.getPath.contains("/ppu/")) Some("CGB or MBC3 test") else if (pngs.isEmpty) Some("no DMG reference") else None
    Rom("mealybug", name(f), f, Screenshot(pngs, Some(LdBB)), 60 * frameCycles, skip)
  }

  def age = files("age-test-roms", ".gb").map { f =>
    val png = sibling(f, base(f) + "-dmgC.png")
    if (png.isDefined) Rom("age", name(f), f, Screenshot(png.toSeq, Some(LdBB)), 60 * frameCycles)
    else Rom("age", name(f), f, Fibonacci(LdBB), 600 * frameCycles, if (base(f).contains("dmgC")) None else Some("another Game Boy model"))
  }

  /** Gambatte runs each test for 15 frames' time; the 16th frame is drawn in the meantime. */
  def gambatte = files("gambatte", ".gb", ".gbc").flatMap { f =>
    val s = base(f)
    val time = 16L * frameCycles
    val out = Seq("dmg08_cgb04c_out", "dmg08_out").find(s.contains).map(o => s.drop(s.indexOf(o) + o.length))
    val png = Seq("_dmg08_cgb04c.png", "_dmg08.png").flatMap(p => sibling(f, s + p)).headOption
    out.map { o =>
      val digits = o.takeWhile(c => Character.digit(c, 16) >= 0)
      if (o.startsWith("audio")) Rom("gambatte", name(f), f, Audio(o.drop(5).startsWith("1")), time + 2 * frameCycles)
      else Rom("gambatte", name(f), f, HexOut(digits), time)
    } ++ png.map(p => Rom("gambatte", name(f) + " (png)", f, Screenshot(Seq(p), None), time))
  }

  /** GBMicrotest ROMs whose sources (aappleby/GBMicrotest) never write a verdict to FF82: scratch
    * programs and experiments rather than tests, which no Game Boy passes. */
  val microNoVerdict = Set(
    "000-oam_lock",
    "000-write_to_x8000",
    "001-vram_unlocked",
    "002-vram_locked",
    "004-tima_boot_phase",
    "004-tima_cycle_timer",
    "007-lcd_on_stat",
    "400-dma",
    "500-scx-timing",
    "800-ppu-latch-scx",
    "801-ppu-latch-scy",
    "802-ppu-latch-tileselect",
    "803-ppu-latch-bgdisplay",
    "audio_testbench",
    "cpu_bus_1",
    "dma_basic",
    "flood_vram",
    "lcdon_write_timing",
    "ly_while_lcd_off",
    "minimal",
    "mode2_stat_int_to_oam_unlock",
    "oam_sprite_trashing",
    "poweron",
    "ppu_scx_vs_bgp",
    "ppu_sprite_testbench",
    "ppu_spritex_vs_scx",
    "ppu_win_vs_wx",
    "ppu_wx_early",
    "temp",
    "toggle_lcdc",
    "wave_write_to_0xC003"
  )

  def micro = files("gbmicrotest", ".gb").map { f =>
    val skip = if (microNoVerdict(base(f))) Some("not a test: writes no verdict") else None
    Rom("gbmicrotest", name(f), f, Micro, (if (f.getName == "is_if_set_during_ime0.gb") 30 else 4) * frameCycles, skip)
  }

  /** A Mooneye suite: the model a test is for is the tag after its last `-` (`G` the DMG). */
  def mooneyeSuite(suite: String, dir: String, check: Check) = files(dir, ".gb").map { f =>
    val n = name(f)
    val tag = base(f).split('-').drop(1).lastOption.getOrElse("")
    val dmg = tag.isEmpty || tag.contains("G") || tag.contains("dmgABC") || tag == "dmg"
    val skip =
      if (!n.startsWith(s"$dir/acceptance") && !n.startsWith(s"$dir/emulator-only")) Some("not an acceptance test")
      else if (!dmg) Some("another Game Boy model")
      else if (n.contains("/mbc2")) Some("MBC2 not modelled")
      else if (n.contains("multicart")) Some("MBC1 multicart wiring not modelled")
      else None
    Rom(suite, n, f, check, 500 * frameCycles, skip)
  }
  def mooneye = mooneyeSuite("mooneye", "mooneye-test-suite", Fibonacci(LdBB))
  def wilbertpol = mooneyeSuite("wilbertpol", "mooneye-test-suite-wilbertpol", Fibonacci(0xED))

  def same = files("same-suite", ".gb").map { f =>
    val n = name(f)
    // Of the APU tests only the two not using the CGB's PCM registers pass on a DMG.
    val skip =
      if (n.contains("/apu/")) { if (base(f).startsWith("div_write_trigger") && !base(f).contains("volume")) None else Some("CGB PCM registers") }
      else if (!n.contains("/interrupt/")) Some("CGB or SGB test")
      else None
    Rom("samesuite", n, f, Fibonacci(LdBB), 600 * frameCycles, skip)
  }

  def screenshots: Seq[Rom] = {
    def shot(suite: String, f: File, png: Option[File], frames: Int, skip: Option[String] = None) =
      Rom(suite, name(f), f, Screenshot(png.toSeq, None), frames.toLong * frameCycles, skip.orElse(if (png.isEmpty) Some("no DMG reference") else None))
    def png(f: File, suffixes: String*) = suffixes.flatMap(s => sibling(f, base(f).replace('-', '_') + s) orElse sibling(f, base(f) + s)).headOption
    files("scribbltests", ".gb").map(f => shot("scribbltests", f, png(f, "-dmg.png", "-cgb-dmg.png"), if (base(f) == "statcount-auto") 300 else 30)) ++
      files("bully", ".gb").map(f => shot("bully", f, sibling(f, "bully.png"), 60)) ++
      files("strikethrough", ".gb").map(f => shot("strikethrough", f, sibling(f, "strikethrough-dmg.png"), 60)) ++
      files("little-things-gb", ".gb").map(f =>
        shot("little-things-gb", f, png(f, "-dmg-cgb.png", "-dmg.png"), 60, if (base(f) == "tellinglys") Some("needs button presses") else None)) ++
      files("turtle-tests", ".gb").map(f => shot("turtle-tests", f, png(f, ".png"), 60)) ++
      files("mbc3-tester", ".gb").map(f => shot("mbc3-tester", f, png(f, "-dmg.png"), 60, Some("MBC3 not modelled"))) ++
      files("rtc3test", ".gb").map(f => shot("rtc3test", f, None, 60, Some("MBC3 RTC not modelled; needs button presses")))
  }

  case class Outcome(passed: Boolean, detail: String, mcycles: Long, frame: Array[Int] = null)

  /** What a check looks at in a running machine. */
  trait View {
    def fib: Seq[Int]
    def pc: Int
    def frame: Array[Int]
    def hram(address: Int): Int
    /** Cartridge RAM, offset from A000. */
    def cart(offset: Int): Int
    def serial: String
    def audioVaried: Boolean
    def audioDone: Boolean
  }

  /** Gambatte's digit tiles: 8 x 8, '#' black, '.' white. */
  val hexTiles: Seq[String] = Seq(
    ".........#######.#.....#.#.....#.#.....#.#.....#.#.....#.#######",
    "............#.......#.......#.......#.......#.......#.......#...",
    ".........#######.......#.......#.#######.#.......#.......#######",
    ".........#######.......#.......#..######.......#.......#.#######",
    ".........#.....#.#.....#.#.....#.#######.......#.......#.......#",
    ".........#######.#.......#.......######........#.......#.######.",
    ".........#######.#.......#.......#######.#.....#.#.....#.#######",
    ".........#######.......#......#......#......#......#.......#....",
    "..........#####..#.....#.#.....#..#####..#.....#.#.....#..#####.",
    ".........#######.#.....#.#.....#.#######.......#.......#.#######",
    "............#.....#...#..#.....#.#######.#.....#.#.....#.#.....#",
    ".........######..#.....#.#.....#.######..#.....#.#.....#.######.",
    "..........#####..#.....#.#.......#.......#.......#.....#..#####.",
    ".........######..#.....#.#.....#.#.....#.#.....#.#.....#.######.",
    ".........#######.#.......#.......#######.#.......#.......#######",
    ".........#######.#.......#.......#######.#.......#.......#......"
  )

  /** The hex digits shown at the top left, `?` where none matches. */
  def hexSeen(frame: Array[Int], n: Int): String = (0 until n).map { i =>
    (0 until 16)
      .find(h => (0 until 64).forall(p => frame((p / 8) * 160 + i * 8 + p % 8) == (if (hexTiles(h)(p) == '#') 3 else 0)))
      .map(h => Character.toUpperCase(Character.forDigit(h, 16)))
      .getOrElse('?')
  }.mkString

  def hexMatches(frame: Array[Int], digits: String): Boolean = digits.zipWithIndex.forall { case (c, i) =>
    val tile = hexTiles(Character.digit(c, 16))
    (0 until 64).forall(p => frame((p / 8) * 160 + i * 8 + p % 8) == (if (tile(p) == '#') 3 else 0))
  }

  private val pngCache = collection.mutable.Map[File, Array[Int]]()
  def reference(png: File) = synchronized { pngCache.getOrElseUpdate(png, GbFrame.load(png)) }
  def screenshotMatch(frame: Array[Int], pngs: Seq[File]) = pngs.map(p => GbFrame.differences(frame, reference(p))).min

  /** The verdict at an event: `opcode` is the opcode check's instruction executing, `frameEnd` a
    * frame (or frame's time) completing, `end` the budget running out. */
  def judge(rom: Rom, v: View, opcode: Boolean, frameEnd: Boolean, end: Boolean, cycles: Long): Option[Outcome] = rom.check match {
    case Fibonacci(_) if opcode =>
      Some(if (v.fib == Seq(3, 5, 8, 13, 21, 34)) Outcome(true, "Fibonacci", cycles)
        else Outcome(false, "registers " + v.fib.map(r => f"$r%02X").mkString(" ") + f" at PC ${v.pc}%04X", cycles))
    case Screenshot(pngs, Some(_)) if opcode =>
      val d = screenshotMatch(v.frame, pngs)
      Some(Outcome(d == 0, if (d == 0) "screenshot" else s"$d pixels differ", cycles))
    case Screenshot(pngs, None) if frameEnd || end =>
      val d = screenshotMatch(v.frame, pngs)
      if (d == 0) Some(Outcome(true, "screenshot", cycles)) else if (end) Some(Outcome(false, s"$d pixels differ", cycles)) else None
    case HexOut(digits) if end =>
      val ok = hexMatches(v.frame, digits)
      Some(Outcome(ok, if (ok) s"out $digits" else s"out ${hexSeen(v.frame, digits.length)}, expected $digits", cycles))
    case Micro if frameEnd || end =>
      v.hram(2) match {
        case 0x01 => Some(Outcome(true, "FF82 = 01", cycles))
        case 0xFF => Some(Outcome(false, f"FF80 = ${v.hram(0)}%02X, expected ${v.hram(1)}%02X", cycles))
        case _ if end => Some(Outcome(false, "timeout", cycles))
        case _ => None
      }
    case Audio(sound) if v.audioDone || end =>
      Some(Outcome(v.audioDone && v.audioVaried == sound, if (!v.audioDone) "no audio" else if (v.audioVaried) "sound" else "silence", cycles))
    case Blargg if frameEnd || end =>
      val signed = v.cart(1) == 0xDE && v.cart(2) == 0xB0 && v.cart(3) == 0x61
      val serial = v.serial.replace('\n', ' ').trim
      if (serial.contains("Passed")) Some(Outcome(true, serial, cycles))
      else if (serial.contains("Failed")) Some(Outcome(false, serial, cycles))
      else if (signed && v.cart(0) == 0) Some(Outcome(true, "A000 = 00", cycles))
      else if (signed && v.cart(0) != 0x80) Some(Outcome(false, f"A000 = ${v.cart(0)}%02X: " + text(v), cycles))
      else if (end) Some(Outcome(false, if (signed) "timeout: " + text(v) else s"timeout; serial: $serial", cycles))
      else None
    case _ if end => Some(Outcome(false, "timeout", cycles))
    case _ => None
  }

  /** Blargg's result text from A004 on. */
  def text(v: View) = Iterator.from(4).take(200).map(v.cart).takeWhile(_ != 0).map(c => if (c == 10) ' ' else c.toChar).mkString.trim

  def opcodeOf(rom: Rom): Int = rom.check match {
    case Fibonacci(o) => o
    case Screenshot(_, Some(o)) => o
    case _ => -1
  }

  /** On `GbReference`, flat-mapped: only ROMs of 32 KB (and their cartridge RAM) fit. */
  def runModel(rom: Rom): Outcome = {
    val image = Files.readAllBytes(rom.file.toPath).map(_ & 0xFF)
    if (image.length > 0x8000) return Outcome(false, "too large for the model", 0)
    val cart = Array.fill(0x10000)(0xFF)
    for (i <- image.indices) cart(i) = image(i)
    val cart0 = cart
    val gb = new GbReference(cart, externalDma = true)
    gb.cpu.reset()
    val view = new View {
      def fib = Seq("B", "C", "D", "E", "H", "L").map(gb.cpu.regs(_).toInt)
      def pc = gb.cpu.pc
      def frame = gb.ppu.frame
      def hram(a: Int) = gb.hram(a)
      def cart(o: Int) = cart0(0xA000 + o)
      def serial = gb.serialOut.map(_.toChar).mkString
      def audioVaried = gb.audioWindows.varied
      def audioDone = gb.audioWindows.done
    }
    var frameEnd = false
    gb.ppu.onFrame = _ => frameEnd = true
    val op = opcodeOf(rom)
    var cycles = 0L
    var nextFrame = frameCycles.toLong
    var outcome: Option[Outcome] = None
    while (outcome.isEmpty) {
      cycles += gb.cpu.step()._2
      // Without the LCD no frames complete; a frame's time counts instead.
      if (cycles >= nextFrame) { nextFrame += frameCycles; if (!gb.ppu.on) frameEnd = true }
      val hit = gb.cpu.opcode == op
      if (hit || frameEnd || gb.audioWindows.done || cycles >= rom.mcycles) outcome = judge(rom, view, hit, frameEnd, cycles >= rom.mcycles, cycles)
      frameEnd = false
    }
    // A failing ROM's screen shows why; it is drawn in the frames after the verdict.
    if (!outcome.get.passed) { val end = cycles + 2 * frameCycles; while (cycles < end) cycles += gb.cpu.step()._2 }
    outcome.get.copy(frame = gb.ppu.frame.clone())
  }

  private val compiled = mutable.Map[GbCartSpec, SimCompiled[RetroCore]]()
  def compile(spec: GbCartSpec) = synchronized {
    compiled.getOrElseUpdate(
      spec,
      FastVerilator(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(1 MHz)))
        .workspacePath(sys.env.getOrElse("SIM_WORKSPACE", "simWorkspace"))
        .workspaceName(f"GameBoyRom_${spec.romBytes}%x_${spec.ramBytes}%x_${spec.mbc}")
        .compile(GameBoy(withTrace = false, cartridge = Some(spec), romDecoder = true))
    )
  }

  /** On the `GameBoy` RTL with a `GbCartridgePlugin`; frames come from its `lcd` pixel port. */
  def runRtl(rom: Rom): Outcome = {
    val image = Files.readAllBytes(rom.file.toPath)
    // Blargg's ROMs write their report to cartridge RAM even when the header declares none
    // (halt_bug), as emulators provide; give every MBC cartridge at least one RAM bank.
    val spec = GbCartSpec.fromHeader(image) match {
      case s if s.mbc != Mbc.None && s.ramBytes < 0x2000 => s.copy(ramBytes = 0x2000)
      case s => s
    }
    val op = opcodeOf(rom)
    var outcome: Outcome = null
    compile(spec).doSimUntilVoid { dut =>
      dut.host[GbCartridgePlugin].load(image)
      dut.host[GbJoypadPlugin].buttons #= 0
      val io = dut.host[BusPlugin].logic.io
      io.rdata #= 0xFF
      io.stall #= false
      val lcd = dut.host[GbPpuPlugin].lcd
      val lcdc = dut.host[GbPpuPlugin].lcdcReg.get
      val ir = dut.host[SequencerService].opcode
      val hramMem = dut.host[GbHramPlugin].logic.mem
      val cartRam = Option(dut.host[GbCartridgePlugin].logic.ram).map(_.mem)
      val cartBytes = spec.ramBytes max 1
      val tx = dut.host[GbSerialPlugin].tx
      val serialText = new StringBuilder
      val rf = dut.host[RegFilePlugin].regs.byReg
      import Sm83Regs._
      val buf = Array.fill(160 * 144)(0)
      val audio = dut.host[GbApuPlugin].audio
      val windows = new AudioWindows
      val view = new View {
        def fib = Seq(B, C, D, E, H, L).map(rf(_).toInt)
        def pc = rf(PC).toInt
        def frame = buf
        def hram(a: Int) = hramMem.getBigInt(a).toInt
        def cart(o: Int) = cartRam.map(_.getBigInt(o % cartBytes).toInt).getOrElse(0xFF)
        def serial = serialText.toString
        def audioVaried = windows.varied
        def audioDone = windows.done
      }
      var cycles = 0L
      var nextFrame = frameCycles.toLong
      var lastY = 0
      var shownAt = Long.MaxValue
      // A pixel every clock; the rest every M-cycle, at the clock ending it.
      val cpuEnable = dut.cpuEnable
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings {
        if (outcome == null || cycles < shownAt) LcdPixel.capture(lcd, buf)
        if ((outcome == null || cycles < shownAt) && cpuEnable.toBoolean) {
          if (tx.valid.toBoolean) serialText += tx.payload.toInt.toChar
          val y = lcd.y.toInt
          val vblank = lastY == Gb.visibleLines - 1 && y == Gb.visibleLines
          var frameEnd = vblank
          lastY = y
          cycles += 1
          if (cycles >= nextFrame) { nextFrame += frameCycles; if ((lcdc.toInt & 0x80) == 0) frameEnd = true }
          if (rom.check.isInstanceOf[Audio]) {
            windows.sample(audio(0).toInt, audio(1).toInt)
            windows.sample(audio(2).toInt, audio(3).toInt)
            if (vblank) windows.vblank()
          }
          val hit = op >= 0 && ir.toInt == op
          if (outcome == null && (hit || frameEnd || windows.done || cycles >= rom.mcycles))
            for (o <- judge(rom, view, hit, frameEnd, cycles >= rom.mcycles, cycles)) {
              outcome = o.copy(frame = buf)
              shownAt = if (o.passed) cycles else cycles + 2 * frameCycles
            }
        }
      }
      dut.clockDomain.waitSamplingWhere(outcome != null && cycles >= shownAt)
      simSuccess()
    }
    outcome
  }
}

/** Runs the c-sp suites and prints a line per ROM and a pass count per suite; also writes them,
  * with every ROM's last screen, as the `RomReport` `gb-<engine>`.
  * {{{ sbt "Test/runMain retro.gb.GbSuiteReport [model|rtl] [name filters...]" }}}
  * `SHARD=i/n` runs every n-th ROM from the i-th, for parallel JVMs. */
object GbSuiteReport extends App {
  val engine = args.headOption.filter(Set("model", "rtl")).getOrElse("rtl")
  val filters = args.dropWhile(Set("model", "rtl"))
  val (shard, shards) = sys.env.get("SHARD").filter(_.nonEmpty).map(_.split('/')).map(a => (a(0).toInt, a(1).toInt)).getOrElse((0, 1))
  val all = GbSuites.roms.filter(r => filters.isEmpty || filters.exists(r.name.contains))
  val selected = all.zipWithIndex.collect { case (r, i) if i % shards == shard => r }
  val report = new RomReport(s"gb-$engine", if (shards > 1) s"-$shard" else "")
  val results = for (rom <- selected) yield {
    val skip = rom.skip.orElse(Some("too large for the model").filter(_ => engine == "model" && rom.file.length > 0x8000))
    val (status, detail) = skip match {
      case Some(why) => ("SKIP", why)
      case None =>
        val o =
          try { if (engine == "model") GbSuites.runModel(rom) else GbSuites.runRtl(rom) }
          catch { case e: Throwable => GbSuites.Outcome(false, s"error: ${e.getMessage}", 0) }
        if (o.frame != null) GbFrame.save(o.frame, report.frame(rom.name))
        rom.check match { case GbSuites.Screenshot(png +: _, _) => report.reference(rom.name, png); case _ => }
        (if (o.passed) "PASS" else "FAIL", f"${o.detail} (${o.mcycles / 1e6}%.2fM)")
    }
    println(f"$status  ${rom.name}%-90s $detail")
    report.add(rom.suite, rom.name, status, detail)
    rom.suite -> status
  }
  report.close()
  for ((suite, rs) <- results.groupBy(_._1).toSeq.sortBy(_._1)) {
    val c = rs.groupBy(_._2).map { case (k, v) => k -> v.size }.withDefaultValue(0)
    println(f"$suite%-18s ${c("PASS")}%5d pass ${c("FAIL")}%5d fail ${c("SKIP")}%5d skipped")
  }
}

/** The CPU and PPU suites on the RTL, a test per ROM (about 15 minutes); `GB_ROMS=a,b` picks the
  * ROMs whose name contains one of them instead. Skipped ROMs are registered as ignored, with the
  * reason in the name. */
class GbSuitesSim extends AnyFunSuite {
  val filters = sys.env
    .get("GB_ROMS")
    .map(_.split(',').toSeq)
    .getOrElse(Seq("blargg/cpu_instrs/individual", "blargg/instr_timing", "blargg/mem_timing", "blargg/halt_bug", "blargg/oam_bug", "mooneye-test-suite/",
        "dmg-acid2"))
  val roms = GbSuites.roms.filter(r => filters.exists(r.name.contains))
  for (rom <- roms) rom.skip match {
    case Some(why) => ignore(s"${rom.name} ($why)") {}
    case None =>
      test(rom.name) {
        val o = GbSuites.runRtl(rom)
        assert(o.passed, o.detail)
      }
  }
  if (roms.isEmpty) test("test ROMs") { cancel(s"no ROMs under ${GbSuites.root}: run scripts/fetch-test-roms.sh") }
}
