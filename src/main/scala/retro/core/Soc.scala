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

/** Implemented by whatever takes interrupt requests to the CPU. Peripherals get a `Bool` per
  * line; several drivers of the same line are ORed. What the level means is the console's: the
  * Game Boy's IF latches a one-cycle pulse, the NES CPU sees the line as driven. */
trait IrqService {
  /** Retain this before calling `request` from a build phase. */
  def irqLock: Retainer
  def request(line: IrqLine): Bool
}

/** The usual shape of a peripheral: one bus slave at `mapping` and the interrupt lines `irqs`.
  * `body` builds the logic once the bus and the interrupt controller hold them. */
abstract class PeripheralPlugin(mapping: AddressMapping, irqs: IrqLine*) extends FiberPlugin {
  def body(slave: BusSlave, irq: Seq[Bool]): Area
  /** Called while the interrupt controller is still open, for requests beyond `irqs`. */
  def whileIrqOpen(): Unit = {}

  val logic = during setup new Area {
    val bus = host[BusService]
    val ctrl = host[IrqService]
    val retainers = Seq(retains(bus.busLock), retains(ctrl.irqLock))
    awaitBuild()
    val slave = bus.newSlave(mapping)
    val lines = irqs.map(ctrl.request)
    whileIrqOpen()
    retainers.foreach(_.release())
    val impl = body(slave, lines)
  }
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
  * @param ramName  names the memory signal (`ram_<ramName>`)
  * @param lockable another plugin (the Game Boy PPU) loads `readLock` and `writeLock`: while
  *                 they are high the bus reads FF and writes are lost. `directRead` ports are
  *                 not affected.
  * @param midCycleRead read the bus port on the falling edge (`MidCycleRead`), so FPGA block RAM
  *                 holds it; `directRead` ports stay combinational. */
class RamPlugin(val ramName: String, val mapping: AddressMapping, val size: Int, val priority: Int = 0,
                val lockable: Boolean = false, val midCycleRead: Boolean = false,
                val init: Int => Int = _ => 0) extends FiberPlugin with DirectRead {
  val readLock, writeLock = Handle[Bool]()
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
    mem.init(Seq.tabulate(1 << width)(i => U(init(i), bus.busConfig.dataWidth bits)))
    mem.simPublic()
    val index = slave.cmd.address(width - 1 downto 0)
    slave.rdata := (if (midCycleRead) MidCycleRead(mem, index) else mem.readAsync(index))
    val writeEnable = Bool()
    writeEnable := slave.writeFire
    mem.write(index, slave.cmd.data, writeEnable)
  }

  /** Apart from `logic`, so the plugin that loads the locks can use `directRead` first. */
  val locking = during build new Area {
    if (lockable) {
      when(readLock.get) { logic.slave.rdata.setAll() }
      when(writeLock.get) { logic.writeEnable := False }
    }
  }
}

/** Answers a constant and ignores writes: unmapped I/O. Low priority by default, so it only gets
  * the addresses no other slave in its range claims. `latched`: answers the data last on the bus
  * instead (the NES's data bus keeps it), refreshed by every completed access, a DMA's too. */
class OpenBusPlugin(val mapping: AddressMapping, val value: BigInt = 0xFF, val priority: Int = -1,
                    val latched: Boolean = false) extends FiberPlugin {
  /** What a read nothing answers returns this cycle. */
  def data: UInt = logic.data
  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(mapping, priority)
    retainer.release()
    val width = bus.busConfig.dataWidth
    val latch = latched generate Reg(UInt(width bits)).init(0)
    if (latched) when(bus.completed.valid) { latch := bus.completed.payload }
    val data = if (latched) latch else U(value, width bits)
    slave.rdata := data
  }
}
