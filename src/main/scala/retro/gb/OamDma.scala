package retro.gb

import retro.core._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

/** OAM (FE00-FE9F) and its DMA (FF46). Writing FF46 copies 160 bytes from `value << 8` into OAM,
  * one per M-cycle, alongside the CPU: the DMA reads its source through `DirectRead` ports,
  * not over the CPU's bus. Following Mooneye's oam_dma tests, the write happens in M-cycle 0,
  * nothing changes in M-cycle 1 (a DMA already running keeps running), and the copy runs in
  * M-cycles 2 to 161; while it runs the CPU reads FF from OAM and its OAM writes are lost.
  * Sources E000-FFFF read WRAM at C000-DFFF. Addresses no `DirectRead` claims read FF.
  *
  * The PPU loads `readLock` and `writeLock`, which block the CPU's OAM accesses in the modes the
  * PPU uses OAM.
  *
  * Not modelled: bus conflicts (on the DMG a CPU access to the bus the DMA is reading returns the
  * DMA's byte). */
class OamDmaPlugin extends FiberPlugin {
  val readLock, writeLock = Handle[Bool]()
  /** A combinational read port into OAM for the PPU, which has its own path to it. */
  def oamRead(index: UInt): UInt = logic.oam.readAsync(index)

  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val oamSlave = bus.newSlave(SizeMapping(0xFE00, 0xA0))
    val dmaSlave = bus.newSlave(SingleMapping(Gb.DMA))
    retainer.release()
    val sources = host.list[DirectRead]

    val oam = Mem.fill(256)(UInt(8 bits)).setName("ram_oam").simPublic()
    oam.init(Seq.fill(256)(U(0, 8 bits)))
    val register = Reg(UInt(8 bits)).init(0xFF).simPublic()
    val startIn = Reg(UInt(2 bits)) init 0
    val active = RegInit(False).simPublic()
    val source = Reg(UInt(8 bits)) init 0
    val index = Reg(UInt(8 bits)) init 0

    // The tick: a DMA started two M-cycles ago takes over from any running one.
    val starting = startIn === 1
    val copying = starting || active
    val from = starting ? register | source
    val at = starting ? U(0, 8 bits) | index
    val address = UInt(16 bits)
    address := from @@ at
    when(from >= 0xE0) { address(13) := False }
    val hits = sources.map(s => s.directMappings.map(_.hit(address)).reduce(_ || _))
    val data = U(0xFF, 8 bits)
    for ((s, hit) <- sources.zip(hits).reverse) when(hit) { data := s.directRead(address) }

    new RetroBusSlaveFactory(dmaSlave).readAndWrite(register, Gb.DMA)
    val oamIndex = oamSlave.cmd.address(7 downto 0)
    oamSlave.rdata := copying ? U(0xFF, 8 bits) | oam.readAsync(oamIndex)
    val cpuWrite = Bool()
    cpuWrite := oamSlave.writeFire
    oam.write(copying ? at | oamIndex, copying ? data | oamSlave.cmd.data, copying || cpuWrite)

    when(startIn =/= 0) { startIn := startIn - 1 }
    when(copying) {
      source := from
      index := at + 1
      active := at =/= 159
    }
    when(dmaSlave.writeFire) { startIn := 2 }
  }

  /** Apart from `logic`, so the PPU can use `oamRead` before it loads the locks. */
  val locking = during build new Area {
    when(readLock.get) { logic.oamSlave.rdata := 0xFF }
    when(writeLock.get) { logic.cpuWrite := False }
  }
}
