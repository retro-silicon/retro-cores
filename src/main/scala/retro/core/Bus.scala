package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._

import scala.collection.mutable.ArrayBuffer

case class RetroBusConfig(addressWidth: Int = 16, dataWidth: Int = 8)

case class BusCmd(c: RetroBusConfig) extends Bundle {
  val write = Bool()
  val address = UInt(c.addressWidth bits)
  val data = UInt(c.dataWidth bits)
}

/** The single external bus of a retro CPU. One `cmd` per M-cycle; `rdata` is sampled at the end
  * of that cycle. The slave raises `stall` to stretch the cycle (wait states, 2A03 RDY). */
case class RetroBus(c: RetroBusConfig) extends Bundle with IMasterSlave {
  val cmd = Flow(BusCmd(c))
  val rdata = UInt(c.dataWidth bits)
  val stall = Bool()

  override def asMaster(): Unit = {
    master(cmd)
    in(rdata, stall)
  }
}

/** A requester's view of the bus. The owner drives `cmd`; `stall` is high when its access did not
  * complete this cycle, because the slave stalled or a higher-priority master took the bus. */
class BusMaster(c: RetroBusConfig, val priority: Int) extends Area {
  val cmd = Flow(BusCmd(c))
  val rdata = UInt(c.dataWidth bits)
  val stall = Bool()
}

trait BusService {
  def busConfig: RetroBusConfig
  /** Retain this before calling `newMaster` from a build phase. */
  def busLock: Retainer
  def newMaster(priority: Int): BusMaster
}

/** Owns the external `RetroBus` and arbitrates between masters by fixed priority (higher wins).
  * The CPU sequencer is one master; DMA engines (2A03 OAM DMA) are others. A master that is not
  * accessing the bus in a cycle is never stalled. */
class BusPlugin(val busConfig: RetroBusConfig = RetroBusConfig()) extends FiberPlugin with BusService {
  val busLock = Retainer()
  private val masters = ArrayBuffer[BusMaster]()
  def newMaster(priority: Int): BusMaster = {
    val m = new BusMaster(busConfig, priority)
    masters += m
    m
  }

  val logic = during build new Area {
    busLock.await()
    val io = master(RetroBus(busConfig))
    val ordered = masters.sortBy(-_.priority)
    val requests = ordered.map(_.cmd.valid)
    val granted = OHMasking.first(requests.asBits())

    io.cmd.valid := requests.orR
    io.cmd.payload := MuxOH.or(granted, ordered.map(_.cmd.payload), bypassIfSingle = true)
    for ((m, i) <- ordered.zipWithIndex) {
      m.rdata := io.rdata
      m.stall := m.cmd.valid && (io.stall || !granted(i))
    }
  }
}
