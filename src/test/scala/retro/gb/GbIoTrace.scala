package retro.gb

import java.io.{File, PrintWriter}
import java.nio.file.Files

/** Logs every CPU access to the I/O registers of `GbReference` running a 32 KB ROM, as
  * `R addr value t` / `CW addr value t` with `t` the M-cycle's end in dots, to diff against the
  * same log from another emulator.
  * {{{ sbt "Test/runMain retro.gb.GbIoTrace rom.gb out.trace mcycles" }}} */
object GbIoTrace extends App {
  val image = Files.readAllBytes(new File(args(0)).toPath).map(_ & 0xFF)
  val cart = Array.fill(0x10000)(0xFF)
  for (i <- 0 until (image.length min 0x8000)) cart(i) = image(i)
  val gb = new GbReference(cart, externalDma = true)
  val out = new PrintWriter(new File(args(1)))
  gb.onIoAccess = (w, a, d) => out.println(f"${if (w) "CW" else "R"} $a%04X $d%02X ${gb.ppu.time}")
  var wasHalted = false
  gb.onStatIrq = () => out.println(s"I ${gb.ppu.ly} ${gb.ppu.time}")
  gb.onAck = i => out.println(s"D $i ${gb.ppu.time} ${if (wasHalted) "H" else ""}")
  gb.cpu.reset()
  var cycles = 0L
  while (cycles < args(2).toLong) { cycles += gb.cpu.step()._2; wasHalted = gb.cpu.halted }
  out.close()
}
