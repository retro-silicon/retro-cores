package retro.gb

import java.io.{File, PrintWriter}
import java.nio.file.Files

/** Writes `GbReference`'s PPU timing for one frame of a 32 KB ROM, in the format of the SameBoy
  * trace patch used to compare dot timing: `I line t` STAT interrupt requests, `W addr value t`
  * PPU register writes, `P line x t` pixels, with `t` in dots.
  * {{{ sbt "Test/runMain retro.gb.GbTrace rom.gb out.trace frame" }}} */
object GbTrace extends App {
  val image = Files.readAllBytes(new File(args(0)).toPath).map(_ & 0xFF)
  val cart = Array.fill(0x10000)(0xFF)
  for (i <- 0 until (image.length min 0x8000)) cart(i) = image(i)
  val gb = new GbReference(cart, externalDma = true)
  val out = new PrintWriter(new File(args(1)))
  val frame = args(2).toLong
  def on = gb.ppu.frames == frame - 1
  gb.onStatIrq = () => if (on) out.println(s"I ${gb.ppu.ly} ${gb.ppu.time}")
  gb.onPpuWrite = (a, d) => if (on) out.println(f"W $a%04X $d%02X ${gb.ppu.time}")
  gb.ppu.onPixel = (y, x, t) => if (on) out.println(s"P $y $x $t")
  gb.ppu.onFrame = _ => if (gb.ppu.frames == frame) out.println(s"V 144 ${gb.ppu.time}")
  gb.cpu.reset()
  while (gb.ppu.frames < frame) gb.cpu.step()
  out.close()
}
