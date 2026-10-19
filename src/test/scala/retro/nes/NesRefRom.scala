package retro.nes

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/** Finds and judges NES test ROMs, for the model runner here and the RTL suite. */
object NesRoms {
  val dir = new File("roms/nes-test-roms")

  /** Every mapper 0/1/3 test ROM under `roms/nes-test-roms`, by path relative to it. */
  def all: Seq[(String, File)] = {
    def walk(f: File): Seq[File] = if (f.isDirectory) f.listFiles().sortBy(_.getName).toSeq.flatMap(walk) else Seq(f)
    if (!dir.isDirectory) Nil
    else walk(dir).filter(_.getName.endsWith(".nes")).map(f => dir.toPath.relativize(f.toPath).toString -> f)
      .filter { case (_, f) =>
        val h = java.nio.file.Files.readAllBytes(f.toPath).take(8).map(_ & 0xFF)
        val mapper = (h(6) >> 4) | (h(7) & 0xF0)
        mapper == 0 || mapper == 1 || mapper == 3
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
  def verdict(nes: NesReference, stableFrames: Int, crcs: Set[String] = Set()): Verdict = {
    val r = nes.prgRam
    lazy val lastLine = screenText(nes).linesIterator.toSeq.lastOption.getOrElse("")
    if (crcs.nonEmpty && stableFrames > 120 && lastLine.matches("[0-9A-F]{8}")) Done(crcs(lastLine), if (crcs(lastLine)) 0 else 1, screenText(nes))
    else if (r(1) == 0xDE && r(2) == 0xB0 && r(3) == 0x61) {
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

  /** A test asking for the reset button (status 81): blargg's tests want it pressed some time
    * after the request, once they sit in their wait loop. */
  def wantsReset(nes: NesReference) = nes.prgRam(0) == 0x81 && nes.prgRam(1) == 0xDE && nes.prgRam(2) == 0xB0
  /** Presses reset 10 frames into each request; call once per frame. */
  class ResetButton(press: () => Unit) {
    private var waited = 0
    def frame(nes: NesReference): Unit = {
      waited = if (wantsReset(nes)) waited + 1 else 0
      if (waited == 10) press()
    }
  }

  /** Tests that only print their results (dmc_dma_during_read4) end with a CRC of them; the
    * header of their source lists the CRCs of the outputs real consoles give. */
  def expectedCrcs(file: File): Set[String] = {
    val source = new File(file.getParentFile, s"source/${file.getName.stripSuffix(".nes")}.s")
    if (!source.isFile) Set()
    else {
      val s = scala.io.Source.fromFile(source)
      try s.getLines().takeWhile(_.startsWith(";")).flatMap("\\b[0-9A-F]{8}\\b".r.findAllIn(_)).toSet finally s.close()
    }
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

/** The APU's output resampled to 44.1 kHz by averaging (a crude low-pass), for WAV files and
  * loudness checks. */
class NesAudio {
  private val out = scala.collection.mutable.ArrayBuffer[Int]()
  private var sum, n = 0L
  private var phase = 0.0
  def add(sample: Int): Unit = {
    sum += sample; n += 1; phase += 44100.0 / 1789773
    if (phase >= 1) { phase -= 1; out += (sum / n).toInt; sum = 0; n = 0 }
  }
  def save(file: File): Unit = {
    val mean = if (out.isEmpty) 0L else out.map(_.toLong).sum / out.size
    val data = out.map(v => math.max(-32768, math.min(32767, (v - mean).toInt)))
    val b = java.nio.ByteBuffer.allocate(44 + 2 * data.size).order(java.nio.ByteOrder.LITTLE_ENDIAN)
    b.put("RIFF".getBytes).putInt(36 + 2 * data.size).put("WAVEfmt ".getBytes).putInt(16).putShort(1).putShort(1)
      .putInt(44100).putInt(88200).putShort(2).putShort(16).put("data".getBytes).putInt(2 * data.size)
    data.foreach(v => b.putShort(v.toShort))
    file.getParentFile.mkdirs()
    java.nio.file.Files.write(file.toPath, b.array())
  }
  /** RMS around the mean, per 4410 samples (a tenth of a second). */
  def loudness: Seq[Double] = rms(out.map(_.toDouble).toSeq)
  private def rms(x: Seq[Double]) = x.grouped(4410).filter(_.size == 4410).map { g =>
    val m = g.sum / g.size
    math.sqrt(g.map(v => (v - m) * (v - m)).sum / g.size)
  }.toSeq
  /** The console's output filters (high-pass at 90 and 440 Hz, low-pass at 14 kHz), first order. */
  def filtered: Seq[Double] = {
    val fs = 44100.0
    def highPass(x: Seq[Double], fc: Double) = {
      val a = 1 / (1 + 2 * math.Pi * fc / fs)
      var (last, y) = (0.0, 0.0)
      x.map { v => y = a * (y + v - last); last = v; y }
    }
    def lowPass(x: Seq[Double], fc: Double) = {
      val w = 2 * math.Pi * fc / fs
      val a = w / (1 + w)
      var y = 0.0
      x.map { v => y += a * (v - y); y }
    }
    lowPass(highPass(highPass(out.map(_.toDouble).toSeq, 90), 440), 14000)
  }
  /** apu_mixer tests beep, play a tone cancelled by the DMC, then beep again: the mean filtered
    * loudness between the beeps, in percent of the beeps'. */
  def residual: Double = {
    val r = rms(filtered)
    val beep = r.max
    val loud = r.indices.filter(i => r(i) > 0.3 * beep)
    val gap = loud.indices.init.find(i => loud(i + 1) - loud(i) > 5).get
    val between = r.slice(loud(gap) + 2, loud(gap + 1) - 1)
    100 * between.sum / between.size / beep
  }
}

/** blargg's apu_mixer tests on `NesReference`, judged by ear made numeric: the residual tone
  * between the beeps must be no louder than on a real console, from the recordings in the
  * nes-test-roms collection (apu_mixer_recordings, analyzed the same way, plus 20%). */
object NesMixerCheck extends App {
  val hardware = Seq("square" -> 1.2, "triangle" -> 1.9, "noise" -> 10.8, "dmc" -> 2.8)
  var ok = true
  for ((name, limit) <- hardware) {
    val file = new File(NesRoms.dir, s"apu_mixer/$name.nes")
    val nes = new NesReference(NesCartridge.load(file))
    val audio = new NesAudio
    nes.onSample = audio.add
    while ((nes.prgRam(0) >= 0x80 || nes.prgRam(1) != 0xDE) && nes.ppu.frames < 3000) nes.runFrames(1)
    val r = audio.residual
    val pass = r <= limit * 1.2
    ok &&= pass
    println(f"${if (pass) "PASS" else "FAIL"}%-5s apu_mixer/$name: residual $r%.1f%% of the beeps (hardware $limit%.1f%%)")
  }
  if (!ok) sys.exit(1)
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
    val crcs = NesRoms.expectedCrcs(file)
    val audio = if (sys.env.contains("WAV")) Some(new NesAudio) else None
    for (a <- audio) nes.onSample = a.add
    var verdict: NesRoms.Verdict = NesRoms.Running
    var stable = 0
    var lastF8 = -1
    val button = new NesRoms.ResetButton(() => nes.reset())
    var frames = 0
    while (verdict == NesRoms.Running && frames < budget) {
      nes.runFrames(1)
      frames += 1
      val f8 = nes.ram(0xF8) << 16 | NesRoms.screenText(nes).hashCode & 0xFFFF
      stable = if (f8 == lastF8) stable + 1 else 0
      lastF8 = f8
      button.frame(nes)
      verdict = NesRoms.verdict(nes, stable, crcs)
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
    for (a <- audio) {
      a.save(new File(s"out/nes/${name.replace('/', '_').stripSuffix(".nes")}.wav"))
      println("    loudness (AC RMS per 1/10 s): " + a.loudness.map(l => f"$l%.0f").mkString(" "))
    }
  }
  println(s"$passed/$total pass")
}
