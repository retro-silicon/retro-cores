package retro.nes

import retro.core._
import spinal.core.sim._

import java.io.File

/** Simulation speed on one ROM, in CPU cycles per second (a frame is 29781):
  * the model alone, the RTL alone, and the two cosimulated.
  * {{{ sbt "Test/runMain retro.nes.NesSpeed" }}} */
object NesSpeed extends App {
  val file = new File(NesRoms.dir, "instr_timing/instr_timing.nes")
  val frames = 100
  val n = frames * 29781
  def time(label: String)(f: => Unit) = {
    val t = System.nanoTime(); f; val s = (System.nanoTime() - t) / 1e9
    println(f"$label%-6s ${n / s / 1000}%5.0f kcycles/s (${frames / s}%.1f frames/s)")
  }
  time("model") { val ref = new NesReference(NesCartridge.load(file)); var c = 0L; while (c < n) c += ref.cpu.step()._2 }
  NesRtl.compile(NesCartSpec.of(NesCartridge.load(file))).doSim { dut =>
    dut.host[NesCartridgePlugin].load(NesCartridge.load(file))
    dut.host[NesApuPlugin].buttons #= 0
    dut.host[NesInterruptsPlugin].logic.get.button #= false
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(10)
    time("rtl") { dut.clockDomain.waitSampling(n) }
  }
  time("cosim") { NesRtl.cosim(file, frames) }
}
