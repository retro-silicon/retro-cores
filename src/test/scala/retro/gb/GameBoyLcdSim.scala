package retro.gb

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import spinal.core._
import spinal.core.sim._

import java.io.File
import java.nio.file.Files

/** Frames from the `GameBoy` RTL's `lcd` pixel port. */
object GbLcd {
  /** Runs `image` on the RTL until `frames` frames are complete and returns the last one. */
  def run(image: Array[Byte], frames: Int): Array[Int] = {
    val spec = CartridgeSpec.fromHeader(image)
    val frame = Array.fill(160 * 144)(0)
    val compiled = SimConfig.withVerilator.addSimulatorFlag("-O3")
      .workspaceName(f"GameBoyLcd_${spec.romBytes}%x_${spec.ramBytes}%x_${spec.mbc}")
      .compile(GameBoy(withTrace = false, cartridge = Some(spec)))
    compiled.doSimUntilVoid { dut =>
      dut.host[CartridgePlugin].load(image)
      dut.host[JoypadPlugin].buttons #= 0
      val io = dut.host[BusPlugin].logic.io
      io.rdata #= 0xFF
      io.stall #= false
      val lcd = dut.host[PpuPlugin].lcd
      var done = 0
      var lastY = 0
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings {
        LcdPixels.capture(lcd, frame)
        val y = lcd.y.toInt
        if (lastY == Gb.visibleLines - 1 && y == Gb.visibleLines) done += 1
        lastY = y
      }
      dut.clockDomain.waitSamplingWhere(done >= frames)
      simSuccess()
    }
    frame
  }
}

/** dmg-acid2 (`scripts/fetch-test-roms.sh` puts it in `roms/dmg-acid2`): the RTL's frame must
  * match the test's reference image and `PpuReference`'s frame pixel for pixel. Saves both to
  * `out/`. */
class GameBoyLcdSim extends AnyFunSuite {
  val dir = new File(GbRomRunner.root, "dmg-acid2")
  val rom = new File(dir, "dmg-acid2.gb")

  test("dmg-acid2") {
    assume(rom.isFile, s"no $rom: run scripts/fetch-test-roms.sh")
    val image = Files.readAllBytes(rom.toPath)
    val frames = 10
    val rtl = GbLcd.run(image, frames)
    GbFrame.save(rtl, new File("out/dmg-acid2-rtl.png"))

    val cart = Array.fill(0x10000)(0xFF)
    for (i <- image.indices) cart(i) = image(i) & 0xFF
    val ref = new GbReference(cart, externalDma = true)
    ref.cpu.reset()
    while (ref.ppu.frames < frames) ref.cpu.step()
    GbFrame.save(ref.ppu.frame, new File("out/dmg-acid2-model.png"))

    assert(GbFrame.differences(rtl, ref.ppu.frame) == 0, "RTL and model frames differ")
    val expected = GbFrame.load(new File(dir, "reference-dmg.png"))
    assert(GbFrame.differences(rtl, expected) == 0, "frame differs from the reference image")
  }
}
