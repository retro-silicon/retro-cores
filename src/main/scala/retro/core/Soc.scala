package retro.core

import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

/** An interrupt request line, declared once by the console (Game Boy `Gb.Irq.TIMER`). */
class IrqLine(val name: String, val index: Int) { override def toString = name }
object IrqLine { def apply(name: String, index: Int) = new IrqLine(name, index) }

/** Implemented by whatever latches interrupt requests (the Game Boy IF register). Peripherals
  * get a `Bool` per line and pulse it for one cycle to raise the request; several drivers of the
  * same line are ORed. */
trait IrqService {
  /** Retain this before calling `request` from a build phase. */
  def irqLock: Retainer
  def request(line: IrqLine): Bool
}

/** A memory that another on-chip block can read without going through the bus, the way a DMA
  * engine with its own path to memory does (Game Boy OAM DMA runs alongside the CPU). */
trait DirectRead {
  /** The bus addresses `directRead` answers. */
  def directMappings: Seq[AddressMapping]
  /** A combinational read port at a bus address. Call it from a build phase. */
  def directRead(address: UInt): UInt
}

/** On-chip RAM as a bus slave. It decodes only the low `log2Up(size)` address bits, so `mapping`
  * must be aligned to that and may cover mirrors (Game Boy echo RAM). Reads are combinational,
  * as the M-cycle bus expects; contents start at zero.
  *
  * @param ramName used for the memory's signal name, so sims can find it */
class RamPlugin(val ramName: String, val mapping: AddressMapping, val size: Int, val priority: Int = 0) extends FiberPlugin with DirectRead {
  def directMappings = Seq(mapping)
  def directRead(address: UInt): UInt = logic.mem.readAsync(address(logic.width - 1 downto 0))

  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(mapping, priority)
    retainer.release()

    val width = log2Up(size)
    val mem = Mem.fill(1 << width)(UInt(bus.busConfig.dataWidth bits)).setName(s"ram_$ramName")
    mem.init(Seq.fill(1 << width)(U(0, bus.busConfig.dataWidth bits)))
    mem.simPublic()
    val index = slave.cmd.address(width - 1 downto 0)
    slave.rdata := mem.readAsync(index)
    mem.write(index, slave.cmd.data, slave.writeFire)
  }
}

/** Answers a constant and ignores writes: unmapped I/O. Low priority by default, so it only gets
  * the addresses no other slave in its range claims. */
class OpenBusPlugin(val mapping: AddressMapping, val value: BigInt = 0xFF, val priority: Int = -1) extends FiberPlugin {
  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(mapping, priority)
    retainer.release()
    slave.rdata := U(value, bus.busConfig.dataWidth bits)
  }
}
