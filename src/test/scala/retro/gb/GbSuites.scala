package retro.gb

import retro.core._
import retro.sm83._
import spinal.core.sim._

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.jdk.CollectionConverters._

/** The test ROMs `scripts/fetch-test-roms.sh` takes from c-sp/game-boy-test-roms into
  * `roms/c-sp`, as a DMG runs them, and how each one reports its result:
  *  - Mealybug Tearoom Tests and AGE's screenshot tests: the frame when `LD B,B` executes must
  *    match the DMG reference screenshot;
  *  - AGE, SameSuite and wilbertpol's Mooneye fork: B C D E H L = 3 5 8 13 21 34 when `LD B,B`
  *    (Mooneye's old `ED`) executes;
  *  - Gambatte: after 15 frames' time, the hex digits in the ROM's name drawn as tiles at the
  *    top left (`_dmg08_out<hex>`), or the frame matching `<name>_dmg08.png`;
  *  - GBMicrotest: FF82 = 01 for a pass, FF for a failure;
  *  - Scribbltests, Bully, Strikethrough, firstwhite, TurtleTests, MBC3 Tester: the frame
  *    matching the DMG screenshot after a fixed time.
  * ROMs for other models, or needing hardware not built (APU, MBC2/3, button presses) are listed
  * with the reason they are skipped. */
object GbSuites {
  val root = new File(GbRomRunner.root, "c-sp")
  val frameCycles = Gb.lineCycles * Gb.frameLines
  val LdBB = 0x40

  sealed trait Check
  case class Fibonacci(opcode: Int) extends Check
  /** The frame must match one of `pngs`: when `opcode` executes, or else at any frame end. */
  case class Screenshot(pngs: Seq[File], opcode: Option[Int]) extends Check
  case class HexOut(digits: String) extends Check
  case object Micro extends Check

  case class Rom(suite: String, name: String, file: File, check: Check, mcycles: Long, skip: Option[String] = None)

  private def files(dir: String, ext: String*): Seq[File] = {
    val d = new File(root, dir)
    if (!d.isDirectory) Nil
    else Files.walk(d.toPath).iterator().asScala.map(_.toFile)
      .filter(f => f.isFile && ext.exists(f.getName.endsWith)).toSeq.sortBy(_.getPath)
  }
  private def base(f: File) = f.getName.take(f.getName.lastIndexOf('.'))
  private def sibling(f: File, name: String) = Some(new File(f.getParentFile, name)).filter(_.isFile)
  private def name(f: File) = root.toPath.relativize(f.toPath).toString.take(root.toPath.relativize(f.toPath).toString.lastIndexOf('.'))

  def roms: Seq[Rom] = mealybug ++ age ++ gambatte ++ micro ++ wilbertpol ++ same ++ screenshots

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
      Rom("gambatte", name(f), f, HexOut(digits), time, if (o.startsWith("audio")) Some("needs the APU") else None)
    } ++ png.map(p => Rom("gambatte", name(f) + " (png)", f, Screenshot(Seq(p), None), time))
  }

  def micro = files("gbmicrotest", ".gb").map { f =>
    Rom("gbmicrotest", name(f), f, Micro, (if (f.getName == "is_if_set_during_ime0.gb") 30 else 4) * frameCycles)
  }

  def wilbertpol = files("mooneye-test-suite-wilbertpol", ".gb").map { f =>
    val n = name(f)
    val tag = base(f).split('-').drop(1).lastOption.getOrElse("")
    val dmg = tag.isEmpty || tag.contains("G") || tag.contains("dmgABC") || tag == "dmg"
    val skip =
      if (!n.startsWith("mooneye-test-suite-wilbertpol/acceptance") && !n.startsWith("mooneye-test-suite-wilbertpol/emulator-only")) Some("not an acceptance test")
      else if (!dmg) Some("another Game Boy model")
      else if (n.contains("boot_hwio")) Some("needs the APU registers")
      else if (n.contains("/mbc2")) Some("MBC2 not modelled")
      else if (n.contains("multicart")) Some("MBC1 multicart wiring not modelled")
      else None
    Rom("wilbertpol", n, f, Fibonacci(0xED), 500 * frameCycles, skip)
  }

  def same = files("same-suite", ".gb").map { f =>
    val n = name(f)
    val skip = if (n.contains("/apu/")) Some("needs the APU") else if (!n.contains("/interrupt/")) Some("CGB or SGB test") else None
    Rom("samesuite", n, f, Fibonacci(LdBB), 600 * frameCycles, skip)
  }

  def screenshots: Seq[Rom] = {
    def shot(suite: String, f: File, png: Option[File], frames: Int, skip: Option[String] = None) =
      Rom(suite, name(f), f, Screenshot(png.toSeq, None), frames.toLong * frameCycles,
        skip.orElse(if (png.isEmpty) Some("no DMG reference") else None))
    def png(f: File, suffixes: String*) = suffixes.flatMap(s => sibling(f, base(f).replace('-', '_') + s) orElse sibling(f, base(f) + s)).headOption
    files("scribbltests", ".gb").map(f => shot("scribbltests", f, png(f, "-dmg.png", "-cgb-dmg.png"), if (base(f) == "statcount-auto") 300 else 30)) ++
      files("bully", ".gb").map(f => shot("bully", f, sibling(f, "bully.png"), 60)) ++
      files("strikethrough", ".gb").map(f => shot("strikethrough", f, sibling(f, "strikethrough-dmg.png"), 60)) ++
      files("little-things-gb", ".gb").map(f => shot("little-things-gb", f, png(f, "-dmg-cgb.png", "-dmg.png"), 60,
        if (base(f) == "tellinglys") Some("needs button presses") else None)) ++
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
    ".........#######.#.......#.......#######.#.......#.......#......")

  /** The hex digits shown at the top left, `?` where none matches. */
  def hexSeen(frame: Array[Int], n: Int): String = (0 until n).map { i =>
    (0 until 16).find(h => (0 until 64).forall(p => frame((p / 8) * 160 + i * 8 + p % 8) == (if (hexTiles(h)(p) == '#') 3 else 0)))
      .map(h => Character.toUpperCase(Character.forDigit(h, 16))).getOrElse('?')
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
    case _ if end => Some(Outcome(false, "timeout", cycles))
    case _ => None
  }

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
    val gb = new GbReference(cart, externalDma = true)
    gb.cpu.reset()
    val view = new View {
      def fib = Seq("B", "C", "D", "E", "H", "L").map(gb.cpu.regs(_).toInt)
      def pc = gb.cpu.pc
      def frame = gb.ppu.frame
      def hram(a: Int) = gb.hram(a)
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
      if (hit || frameEnd || cycles >= rom.mcycles) outcome = judge(rom, view, hit, frameEnd, cycles >= rom.mcycles, cycles)
      frameEnd = false
    }
    // A failing ROM's screen shows why; it is drawn in the frames after the verdict.
    if (!outcome.get.passed) { val end = cycles + 2 * frameCycles; while (cycles < end) cycles += gb.cpu.step()._2 }
    outcome.get.copy(frame = gb.ppu.frame.clone())
  }

  /** On the `GameBoy` RTL with a `CartridgePlugin`; frames come from its `lcd` pixel port. */
  def runRtl(rom: Rom): Outcome = {
    val image = Files.readAllBytes(rom.file.toPath)
    val spec = CartridgeSpec.fromHeader(image) match {
      case s if s.mbc != Mbc.None && s.ramBytes < 0x2000 => s.copy(ramBytes = 0x2000)
      case s => s
    }
    val op = opcodeOf(rom)
    var outcome: Outcome = null
    GbRomRunner.compile(spec).doSimUntilVoid { dut =>
      dut.host[CartridgePlugin].load(image)
      dut.host[JoypadPlugin].buttons #= 0
      val io = dut.host[BusPlugin].logic.io
      io.rdata #= 0xFF
      io.stall #= false
      val lcd = dut.host[PpuPlugin].lcd
      val lcdc = dut.host[PpuPlugin].lcdcReg
      val ir = dut.host[SequencerPlugin].logic.ir
      val hramMem = dut.host.list[RamPlugin].find(_.ramName == "hram").get.logic.mem
      val rf = dut.host[RegFilePlugin].regs.byReg
      import Sm83Regs._
      val buf = Array.fill(160 * 144)(0)
      val view = new View {
        def fib = Seq(B, C, D, E, H, L).map(rf(_).toInt)
        def pc = rf(PC).toInt
        def frame = buf
        def hram(a: Int) = hramMem.getBigInt(a).toInt
      }
      var cycles = 0L
      var nextFrame = frameCycles.toLong
      var lastY = 0
      var shownAt = Long.MaxValue
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings {
        if (outcome == null || cycles < shownAt) {
          LcdPixels.capture(lcd, buf)
          val y = lcd.y.toInt
          var frameEnd = lastY == Gb.visibleLines - 1 && y == Gb.visibleLines
          lastY = y
          cycles += 1
          if (cycles >= nextFrame) { nextFrame += frameCycles; if ((lcdc.toInt & 0x80) == 0) frameEnd = true }
          val hit = op >= 0 && ir.toInt == op
          if (outcome == null && (hit || frameEnd || cycles >= rom.mcycles))
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

/** Runs the c-sp suites and prints a line per ROM and a pass count per suite; also writes them to
  * `out/gb-suites-<engine>.tsv`.
  * {{{ sbt "Test/runMain retro.gb.GbSuiteReport [model|rtl] [name filters...]" }}}
  * `SHARD=i/n` runs every n-th ROM from the i-th, for parallel JVMs. */
object GbSuiteReport extends App {
  val engine = args.headOption.filter(Set("model", "rtl")).getOrElse("rtl")
  val filters = args.dropWhile(Set("model", "rtl"))
  val (shard, shards) = sys.env.get("SHARD").map(_.split('/')).map(a => (a(0).toInt, a(1).toInt)).getOrElse((0, 1))
  val all = GbSuites.roms.filter(r => filters.isEmpty || filters.exists(r.name.contains))
  val selected = all.zipWithIndex.collect { case (r, i) if i % shards == shard => r }
  val suffix = if (shards > 1) s"-$shard" else ""
  new File("out").mkdirs()
  val log = new PrintWriter(new File(s"out/gb-suites-$engine$suffix.tsv"))
  val results = for (rom <- selected) yield {
    val (status, detail) = rom.skip match {
      case Some(why) => ("SKIP", why)
      case None =>
        val o = try { if (engine == "model") GbSuites.runModel(rom) else GbSuites.runRtl(rom) }
        catch { case e: Throwable => GbSuites.Outcome(false, s"error: ${e.getMessage}", 0) }
        if (!o.passed && o.frame != null) GbFrame.save(o.frame, new File(s"out/frames-$engine/${rom.name}.png"))
        (if (o.passed) "PASS" else "FAIL", f"${o.detail} (${o.mcycles / 1e6}%.2fM)")
    }
    println(f"$status  ${rom.name}%-90s $detail")
    log.println(s"${rom.suite}\t${rom.name}\t$status\t$detail")
    log.flush()
    rom.suite -> status
  }
  log.close()
  for ((suite, rs) <- results.groupBy(_._1).toSeq.sortBy(_._1)) {
    val c = rs.groupBy(_._2).map { case (k, v) => k -> v.size }.withDefaultValue(0)
    println(f"$suite%-18s ${c("PASS")}%5d pass ${c("FAIL")}%5d fail ${c("SKIP")}%5d skipped")
  }
}
