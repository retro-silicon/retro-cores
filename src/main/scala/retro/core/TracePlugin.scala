package retro.core

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.plugin._

import scala.collection.mutable.ArrayBuffer

/** Whitebox for the sequencer-based CPUs: publishes, as `simPublic` signals, everything a probe
  * needs to rebuild what happened in each M-cycle (opcode, step, bus access, completion) and the
  * register file. Observes only; adding or removing it changes no other logic. */
class TracePlugin extends FiberPlugin {
  val logic = during build new Area {
    val seq = host[SequencerPlugin]
    val rf = host[RegFilePlugin]
    val s = seq.logic.get
    val m = s.master

    def tap[T <: Data](name: String, that: T): T = CombInit(that).setName(s"trace_$name").simPublic()
    val opcode = tap("opcode", s.ir)
    val step = tap("step", s.step)
    val done = tap("done", s.done)
    val fetch = tap("fetch", s.fetch)
    val prefix = tap("prefix", s.prefix)
    val busValid = tap("bus_valid", m.cmd.valid)
    val busWrite = tap("bus_write", m.cmd.write)
    val busAddress = tap("bus_address", m.cmd.address)
    val busWdata = tap("bus_wdata", m.cmd.data)
    val busRdata = tap("bus_rdata", m.rdata)
    val regs = rf.spec.regs.map(r => r.name -> tap(s"reg_${r.name}", rf.regs.byReg(r)))
    // The same values packed into words of at most 64 bits, so a probe reads a cycle's bus with
    // one simulator access and a retire's registers with a few (rdata in the low bits; registers
    // in order from the low bits of each word).
    val cycle = tap("cycle", busValid ## busWrite ## done ## fetch ## prefix ## busAddress ## busWdata ## busRdata)
    val regWords: Seq[(Bits, Seq[(String, Int, Int)])] = {
      val groups = ArrayBuffer(ArrayBuffer[(String, Data)]())
      for ((n, r) <- regs) {
        if (groups.last.map(_._2.getBitsWidth).sum + r.getBitsWidth > 64) groups += ArrayBuffer()
        groups.last += n -> r
      }
      for ((g, i) <- groups.toSeq.zipWithIndex) yield {
        val fields = g.scanLeft(("", 0, 0)) { case ((_, o, w), (n, r)) => (n, o + w, r.getBitsWidth) }.tail.toSeq
        tap(s"regs_$i", Cat(g.map(_._2))) -> fields
      }
    }
  }
}
