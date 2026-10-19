package retro.gb

import java.nio.file.Files

/** Runs test ROMs on `GbReference` alone, without the RTL: seconds instead of minutes, for trying
  * a timing hypothesis in the model before changing the RTL. ROMs larger than 32 KB are mapped
  * flat (no MBC), so this is for the ROM-only Mooneye tests and the 32 KB Blargg ones.
  *
  * {{{ sbt "Test/runMain retro.gb.GbRefRom timer/ halt_ime" }}}
  * prints PASS / FAIL / timeout for each ROM whose name contains an argument. */
object GbRefRom extends App {
  val budget = sys.env.getOrElse("STEPS", "30000000").toLong
  for ((name, file, _) <- GbRomRunner.roms if args.exists(name.contains)) {
    val image = Files.readAllBytes(file.toPath).map(_ & 0xFF)
    val cart = Array.fill(0x10000)(0xFF)
    for (i <- 0 until (image.length min 0x8000)) cart(i) = image(i)
    val gb = new GbReference(cart, externalDma = true)
    gb.cpu.reset()
    var verdict = "timeout"
    var steps = 0L
    var last: (Seq[Int], Int) = null
    while (verdict == "timeout" && steps < budget) {
      gb.cpu.step()
      steps += 1
      if (steps % 1000 == 0) {
        // Mooneye ends in LD B,B / JR -2, so registers and PC stop changing; Blargg writes a
        // status byte and the DE B0 61 signature at A000.
        val r = gb.cpu.regs
        val now = (Seq("B", "C", "D", "E", "H", "L").map(r(_).toInt), r("PC").toInt)
        if (now == last && now._1 == Seq(3, 5, 8, 13, 21, 34)) verdict = "PASS"
        else if (now == last && now._1.forall(_ == 0x42)) verdict = "FAIL"
        else if (cart(0xA001) == 0xDE && cart(0xA002) == 0xB0 && cart(0xA003) == 0x61 && cart(0xA000) != 0x80)
          verdict = if (cart(0xA000) == 0) "PASS" else "FAIL"
        last = now
      }
    }
    println(f"$verdict%-8s $name" + (if (verdict == "timeout") f" (PC ${gb.cpu.pc}%04X, LCDC ${gb.ppu.lcdc}%02X, LY ${gb.ppu.ly})" else ""))
    // Blargg's text output, for the failing subtest's number and printout.
    if (verdict != "PASS" && cart(0xA001) == 0xDE)
      println(Iterator.from(0xA004).map(cart).takeWhile(_ != 0).map(_.toChar).mkString.linesIterator.map("    " + _).mkString("\n"))
  }
}
