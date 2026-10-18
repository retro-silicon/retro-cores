package retro.demo

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable

class DemoCoreSim extends AnyFunSuite {
  test("plugins compose, halts lose nothing, whitebox sees every stage") {
    SimConfig.withVerilator.compile(DemoCore()).doSim(seed = 42) { dut =>
      val halt = dut.host[HaltPlugin].logic.stall
      val sink = dut.host[SinkPlugin].logic.result
      val wb = dut.host[WhiteboxPlugin].logic
      val offset = DemoCore.amounts.map(_._2).sum

      dut.clockDomain.forkStimulus(10)
      halt #= false
      dut.clockDomain.waitSampling()

      val seen = mutable.ArrayBuffer[Int]()
      var stageValidSeen = Set[Int]()
      for (_ <- 0 until 400) {
        halt.randomize()
        dut.clockDomain.waitSampling()
        if (sink.valid.toBoolean) seen += sink.payload.toInt
        stageValidSeen ++= wb.valid.collect { case (id, v) if v.toBoolean => id }
      }

      assert(seen.size > 50, s"too few results: ${seen.size}")
      val expected = seen.indices.map(i => (i + offset) & 0xff)
      assert(seen == expected, "results skipped, duplicated or mis-added under random halts")
      assert(stageValidSeen == wb.valid.keySet.toSet, "whitebox never saw some stages valid")
    }
  }
}
