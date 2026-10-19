package retro.gb

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Files
import javax.imageio.ImageIO

/** Game Boy frames as greyscale PNGs, with the shades dmg-acid2's reference image uses
  * (00 55 AA FF), and comparison against a reference image. */
object GbFrame {
  val grey = Array(0xFF, 0xAA, 0x55, 0x00)

  def save(frame: Array[Int], file: File): Unit = {
    val img = new BufferedImage(160, 144, BufferedImage.TYPE_INT_RGB)
    for (y <- 0 until 144; x <- 0 until 160) {
      val g = grey(frame(y * 160 + x))
      img.setRGB(x, y, 0xFF000000 | g << 16 | g << 8 | g)
    }
    file.getParentFile.mkdirs()
    ImageIO.write(img, "png", file)
  }

  /** Shades of a 160 x 144 PNG in those greys. */
  def load(file: File): Array[Int] = {
    val img = ImageIO.read(file)
    Array.tabulate(160 * 144)(i => grey.indexOf(img.getRGB(i % 160, i / 160) & 0xFF))
  }

  def differences(a: Array[Int], b: Array[Int]): Int = a.indices.count(i => a(i) != b(i))
}

/** Runs a 32 KB ROM on `GbReference` for some frames and saves the last one:
  * {{{ sbt "Test/runMain retro.gb.GbRefFrame roms/acid/dmg-acid2.gb out.png 20 [reference.png]" }}} */
object GbRefFrame extends App {
  val image = Files.readAllBytes(new File(args(0)).toPath).map(_ & 0xFF)
  val cart = Array.fill(0x10000)(0xFF)
  for (i <- 0 until (image.length min 0x8000)) cart(i) = image(i)
  val gb = new GbReference(cart, externalDma = true)
  gb.cpu.reset()
  val frames = if (args.length > 2) args(2).toInt else 20
  while (gb.ppu.frames < frames) gb.cpu.step()
  GbFrame.save(gb.ppu.frame, new File(args(1)))
  if (args.length > 3) println(s"${GbFrame.differences(gb.ppu.frame, GbFrame.load(new File(args(3))))} pixels differ from ${args(3)}")
}
