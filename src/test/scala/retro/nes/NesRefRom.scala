package retro.nes

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/** Finds and judges NES test ROMs, for the model runner here and the RTL suite. */
object NesRoms {
  val dir = new File("roms/nes-test-roms")

  /** Every mapper 0/3 test ROM under `roms/nes-test-roms`, by path relative to it. */
  def all: Seq[(String, File)] = {
    def walk(f: File): Seq[File] = if (f.isDirectory) f.listFiles().sortBy(_.getName).toSeq.flatMap(walk) else Seq(f)
    if (!dir.isDirectory) Nil
    else walk(dir).filter(_.getName.endsWith(".nes")).map(f => dir.toPath.relativize(f.toPath).toString -> f)
      .filter { case (_, f) =>
        val h = java.nio.file.Files.readAllBytes(f.toPath).take(8).map(_ & 0xFF)
        val mapper = (h(6) >> 4) | (h(7) & 0xF0)
        mapper == 0 || mapper == 3
      }
  }

  sealed trait Verdict
  case object Running extends Verdict
  case class Done(passed: Boolean, code: Int, text: String) extends Verdict

  /** The test protocols: newer blargg ROMs write a status byte and the signature DE B0 61 at
    * 6000 (status 80 while running, 81 when they want a reset, else the result) and their text
    * from 6004; older ones leave the result in zero page F8 (1 = passed) and print to the
    * screen. `stableFrames` is how long F8 and the screen have been unchanged (some tests idle for 80
    * frames mid-run). */
  def verdict(nes: NesReference, stableFrames: Int): Verdict = {
    val r = nes.prgRam
    if (r(1) == 0xDE && r(2) == 0xB0 && r(3) == 0x61) {
      if (r(0) >= 0x80) Running
      else Done(r(0) == 0, r(0), Iterator.from(4).map(r).takeWhile(_ != 0).map(_.toChar).mkString.trim)
    } else if (stableFrames > 120) {
      // Older ROMs: zero page F8 (1 = passed), or only the screen: "$01" for the 2005 PPU
      // tests, "Passed"/"Failed" or "PASSED"/"FAILED" for others.
      val text = screenText(nes)
      val code = """(?m)^\$([0-9A-F]{2})$""".r.findFirstMatchIn(text).map(m => Integer.parseInt(m.group(1), 16))
      if (nes.ram(0xF8) != 0) Done(nes.ram(0xF8) == 1, nes.ram(0xF8), text)
      else if (code.nonEmpty) Done(code.get == 1, code.get, text)
      else if (text.toLowerCase.contains("passed")) Done(true, 0, text)
      else if (text.toLowerCase.contains("failed")) Done(false, 1, text)
      else Running
    } else Running
  }

  /** The text on nametable 0, assuming the font maps tiles to ASCII as blargg's tests do. */
  def screenText(nes: NesReference): String =
    (0 until 30).map(row => (0 until 32).map(c => nes.ppu.ciram(row * 32 + c)).map(t => if (t >= 0x20 && t < 0x7F) t.toChar else ' ').mkString.trim)
      .filter(_.nonEmpty).mkString("\n")

  /** The 2C02 palette (as most emulators ship it), for saving frames. */
  val rgb = Array(
    0x666666, 0x002A88, 0x1412A7, 0x3B00A4, 0x5C007E, 0x6E0040, 0x6C0600, 0x561D00, 0x333500, 0x0B4800, 0x005200, 0x004F08, 0x00404D, 0, 0, 0,
    0xADADAD, 0x155FD9, 0x4240FF, 0x7527FE, 0xA01ACC, 0xB71E7B, 0xB53120, 0x994E00, 0x6B6D00, 0x388700, 0x0C9300, 0x008F32, 0x007C8D, 0, 0, 0,
    0xFFFEFF, 0x64B0FF, 0x9290FF, 0xC676FF, 0xF36AFF, 0xFE6ECC, 0xFE8170, 0xEA9E22, 0xBCBE00, 0x88D800, 0x5CE430, 0x45E082, 0x48CDDE, 0x4F4F4F, 0, 0,
    0xFFFEFF, 0xC0DFFF, 0xD3D2FF, 0xE8C8FF, 0xFBC2FF, 0xFEC4EA, 0xFECCC5, 0xF7D8A5, 0xE4E594, 0xCFEF96, 0xBDF4AB, 0xB3F3CC, 0xB5EBF2, 0xB8B8B8, 0, 0)

  def save(frame: Array[Int], file: File): Unit = {
    val img = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
    for (y <- 0 until 240; x <- 0 until 256) img.setRGB(x, y, 0xFF000000 | rgb(frame(y * 256 + x) & 0x3F))
    file.getParentFile.mkdirs()
    ImageIO.write(img, "png", file)
  }
}

/** Runs NES test ROMs on `NesReference` alone, in seconds:
  * {{{ sbt "Test/runMain retro.nes.NesRefRom ppu_vbl_nmi/rom_singles sprite_hit" }}}
  * prints PASS / FAIL / timeout for each ROM whose path contains an argument, with the test's
  * text, and saves its last frame to out/nes/. `DOTS` sets `dotsBefore`, `FRAMES` the budget. */
object NesRefRom extends App {
  val budget = sys.env.getOrElse("FRAMES", "2000").toInt
  val dots = sys.env.getOrElse("DOTS", "2").toInt
  var passed, total = 0
  for ((name, file) <- NesRoms.all if args.isEmpty || args.exists(name.contains)) {
    val nes = new NesReference(NesCartridge.load(file), dots)
    var verdict: NesRoms.Verdict = NesRoms.Running
    var stable = 0
    var lastF8 = -1
    var frames = 0
    while (verdict == NesRoms.Running && frames < budget) {
      nes.runFrames(1)
      frames += 1
      val f8 = nes.ram(0xF8) << 16 | NesRoms.screenText(nes).hashCode & 0xFFFF
      stable = if (f8 == lastF8) stable + 1 else 0
      lastF8 = f8
      // A reset request (81): press reset after a few frames.
      if (nes.prgRam(0) == 0x81 && nes.prgRam(1) == 0xDE && stable > 10) { nes.prgRam(0) = 0x80; nes.cpu.reset() }
      verdict = NesRoms.verdict(nes, stable)
    }
    total += 1
    val text = verdict match {
      case NesRoms.Done(true, _, _) => passed += 1; "PASS"
      case NesRoms.Done(false, code, _) => s"FAIL $code"
      case NesRoms.Running => "timeout"
    }
    println(f"$text%-9s $name ($frames frames)")
    verdict match {
      case NesRoms.Done(true, _, _) =>
      case NesRoms.Done(false, _, t) => println(t.linesIterator.map("    " + _).mkString("\n"))
      case _ => println(NesRoms.screenText(nes).linesIterator.map("    " + _).mkString("\n"))
    }
    NesRoms.save(nes.ppu.frame, new File(s"out/nes/${name.replace('/', '_').stripSuffix(".nes")}.png"))
  }
  println(s"$passed/$total pass")
}
