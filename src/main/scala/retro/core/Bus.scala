package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.misc._
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

/** A responder on the bus: RAM, a peripheral's registers, open bus. `cmd.valid` is high in every
  * cycle where the granted master's access decodes to this slave; the slave answers `rdata`
  * combinationally in the same cycle and may raise `stall` to stretch it. A write takes effect
  * on `fire`, the cycle it is not stalled. */
class BusSlave(c: RetroBusConfig, val mapping: AddressMapping, val priority: Int) extends Area {
  val cmd = Flow(BusCmd(c))
  val rdata = UInt(c.dataWidth bits)
  val stall = Bool()
  stall := False
  def fire = cmd.valid && !stall
  def writeFire = fire && cmd.write
  def readFire = fire && !cmd.write
  def writes(address: Int) = writeFire && cmd.address === address
  def reads(address: Int) = readFire && cmd.address === address
}

trait BusService {
  def busConfig: RetroBusConfig
  /** Retain this before calling `newMaster` or `newSlave` from a build phase. */
  def busLock: Retainer
  def newMaster(priority: Int): BusMaster
  /** A slave answering the addresses `mapping` hits. Where mappings overlap, the highest
    * priority wins, so a catch-all (open bus) is a low-priority slave over a wide range. */
  def newSlave(mapping: AddressMapping, priority: Int = 0): BusSlave
  /** Lets a block take over accesses (Game Boy OAM DMA bus conflicts): see `BusIntercept`. */
  def newIntercept(): BusIntercept
  /** The access on the bus this cycle, the granted master's, for blocks that watch it (DMA bus
    * conflicts, the OAM bug). Usable from a build phase that holds no `busLock`. */
  def access: Flow[BusCmd]
  /** The access completing this cycle, if any, with the data on the bus: the byte written or
    * the byte read. Usable from a build phase that holds no `busLock`. */
  def completed: Flow[UInt]
}

/** While `hit` is high the bus access goes to no slave and a read returns `data`. The owner
  * decodes the access from `BusService.access`. */
class BusIntercept(c: RetroBusConfig) extends Area {
  val hit = Bool()
  val data = UInt(c.dataWidth bits)
}

/** Owns the bus: arbitrates masters by fixed priority (higher wins) and decodes addresses to
  * slaves. The CPU sequencer is one master; DMA engines (2A03 OAM DMA) are others. A master that
  * is not accessing the bus in a cycle is never stalled.
  *
  * Addresses no slave claims go to the external `RetroBus` port `logic.io`, so a bare CPU (no
  * slaves at all) exposes its whole bus, and an SoC exposes only what is really off-chip, such
  * as a Game Boy cartridge. With `external = false` unclaimed reads return `openBus`. */
class BusPlugin(val busConfig: RetroBusConfig = RetroBusConfig(),
                val external: Boolean = true,
                val openBus: BigInt = 0xFF) extends FiberPlugin with BusService {
  val busLock = Retainer()
  private val masters = ArrayBuffer[BusMaster]()
  private val slaves = ArrayBuffer[BusSlave]()
  def newMaster(priority: Int): BusMaster = {
    val m = new BusMaster(busConfig, priority)
    masters += m
    m
  }
  def newSlave(mapping: AddressMapping, priority: Int = 0): BusSlave = {
    val s = new BusSlave(busConfig, mapping, priority)
    slaves += s
    s
  }
  def access = logic.cmd
  def completed = logic.completed
  private val intercepts = ArrayBuffer[BusIntercept]()
  def newIntercept(): BusIntercept = {
    val i = new BusIntercept(busConfig)
    intercepts += i
    i
  }

  val logic = during build new Area {
    busLock.await()
    val ordered = masters.sortBy(-_.priority)
    val requests = ordered.map(_.cmd.valid)
    val granted = OHMasking.first(requests.asBits())
    val cmd = Flow(BusCmd(busConfig))
    cmd.valid := requests.orR
    cmd.payload := MuxOH.or(granted, ordered.map(_.cmd.payload), bypassIfSingle = true)

    // ---- Address decode ------------------------------------------------------------------
    val decoded = slaves.sortBy(-_.priority)
    val hits = decoded.map(_.mapping.hit(cmd.address))
    val select = if (decoded.isEmpty) B(0, 0 bits) else OHMasking.first(hits.asBits())
    val unclaimed = if (decoded.isEmpty) True else !hits.orR
    val intercepted = intercepts.map(_.hit).foldLeft(False: Bool)(_ || _)
    for ((s, i) <- decoded.zipWithIndex) {
      s.cmd.valid := cmd.valid && select(i) && !intercepted
      s.cmd.payload := cmd.payload
    }
    val io = external generate master(RetroBus(busConfig))
    val outside = external generate new Area {
      io.cmd.valid := cmd.valid && unclaimed && !intercepted
      io.cmd.payload := cmd.payload
    }
    val (missData, missStall) = if (external) (io.rdata, io.stall) else (U(openBus, busConfig.dataWidth bits), False)
    val decodedData = if (decoded.isEmpty) missData else unclaimed ? missData | MuxOH.or(select, decoded.map(_.rdata), bypassIfSingle = true)
    val decodedStall = if (decoded.isEmpty) missStall else unclaimed ? missStall | (select & decoded.map(_.stall).asBits()).orR
    val rdata = UInt(busConfig.dataWidth bits)
    rdata := decodedData
    for (i <- intercepts.reverse) when(i.hit) { rdata := i.data }
    val stall = decodedStall && !intercepted

    val completed = Flow(UInt(busConfig.dataWidth bits))
    completed.valid := cmd.valid && !stall
    completed.payload := cmd.write ? cmd.data | rdata

    for ((m, i) <- ordered.zipWithIndex) {
      m.rdata := rdata
      m.stall := m.cmd.valid && (stall || !granted(i))
    }
  }
}

/** SpinalHDL's `BusSlaveFactory` on a `BusSlave`, so peripheral registers are declared the usual
  * SpinalHDL way (`read`, `write`, `createReadAndWrite`, `onWrite`...). Addresses are absolute.
  * Read bits no register claims return `idleBits` (the Game Boy returns 1 for unused bits). */
class RetroBusSlaveFactory(slave: BusSlave, idleBits: BigInt = 0) extends BusSlaveFactoryDelayed {
  val dataWidth = slave.rdata.getWidth
  val readData = Bits(dataWidth bits)
  readData := B(idleBits, dataWidth bits)
  slave.rdata := readData.asUInt

  val askWrite = (slave.cmd.valid && slave.cmd.write).allowPruning()
  val askRead = (slave.cmd.valid && !slave.cmd.write).allowPruning()
  val doWrite = slave.writeFire.allowPruning()
  val doRead = slave.readFire.allowPruning()

  override def readAddress() = slave.cmd.address
  override def writeAddress() = slave.cmd.address
  override def readHalt() = slave.stall := True
  override def writeHalt() = slave.stall := True
  override def busDataWidth = dataWidth
  override def wordAddressInc = 1

  override def build(): Unit = {
    super.doNonStopWrite(slave.cmd.data.asBits)
    def doMapped(jobs: scala.collection.Seq[BusSlaveFactoryElement]) =
      super.doMappedElements(jobs, askWrite, askRead, doWrite, doRead, slave.cmd.data.asBits, readData)
    switch(slave.cmd.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping])
        is(address.asInstanceOf[SingleMapping].address) { doMapped(jobs) }
    }
    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping])
      when(address.hit(slave.cmd.address)) { doMapped(jobs) }
  }
}
