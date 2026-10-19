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
  * The DMG OAM corruption bug (`GbReference.oamBug`, after Pan Docs): an address in FE00-FEFF on
  * the CPU's address bus, or stepped by its IDU, while the PPU scans OAM rewrites the row the scan
  * reads (`scanRow`, loaded by the PPU) from the rows before it.
  *
  * No OAM bug while a DMA copies.
  *
  * Bus conflicts (`conflicts`): while the DMA copies, a CPU access to the bus the DMA reads from
  * (VRAM, or the external bus for everything else below FE00) goes nowhere: a read returns the
  * byte being copied, and a write replaces it in OAM, or for a source in A000-FDFF ANDs into it
  * (Gambatte's oamdma tests). FEA0-FEFF reads FF. */
class OamDmaPlugin extends FiberPlugin {
  val readLock, writeLock = Handle[Bool]()
  /** The 8-byte OAM row the PPU's scan reads this M-cycle, 0 when it reads none (row 0 is never
    * corrupted). */
  val scanRow = Handle[UInt]()
  /** A combinational read port into OAM for the PPU, which has its own path to it. */
  def oamRead(index: UInt): UInt = logic.oam(index)

  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val oamSlave = bus.newSlave(SizeMapping(0xFE00, 0xA0))
    val dmaSlave = bus.newSlave(SingleMapping(Gb.DMA))
    val intercept = bus.newIntercept()
    retainer.release()
    val sources = host.list[DirectRead]

    // Registers rather than a RAM: the OAM bug rewrites up to three rows in one M-cycle.
    val oam = Vec.fill(Gb.oamBytes)(Reg(UInt(8 bits)) init 0)
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
    oamSlave.rdata := copying ? U(0xFF, 8 bits) | oam(oamIndex)
    val cpuWrite = Bool()
    cpuWrite := oamSlave.writeFire
    when(copying || cpuWrite) { oam(copying ? at | oamIndex) := (copying ? data | oamSlave.cmd.data) }

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

  val conflicts = during build new Area {
    val cmd = host[BusPlugin].logic.cmd
    val copying = logic.copying
    val at = logic.at
    val data = logic.data
    def isVram(a: UInt) = a(15 downto 13) === 4
    val raw = logic.from @@ at
    val conflict = copying && cmd.valid && cmd.address < 0xFE00 && isVram(cmd.address) === isVram(raw)
    logic.intercept.hit := conflict || copying && cmd.valid && cmd.address >= 0xFEA0 && cmd.address < 0xFF00
    logic.intercept.data := conflict ? data | U(0xFF, 8 bits)
    val anded = raw >= 0xA000 && !isVram(raw)
    when(conflict && cmd.write) { logic.oam(at) := anded ? (data & cmd.data) | cmd.data }
  }

  val bug = during build new Area {
    val cmd = host[BusPlugin].logic.cmd
    val isa = host[retro.sm83.Sm83.IsaPlugin]
    val oam = logic.oam
    val row = scanRow.get
    val address = cmd.valid ? cmd.address | isa.iduAddress.get
    val hit = (cmd.valid || isa.iduActive.get) && address(15 downto 8) === 0xFE && row =/= 0 && !logic.copying
    val read = cmd.valid && !cmd.write
    val increase = isa.iduActive.get

    def w(r: Int, i: Int) = oam(8 * r + 2 * i + 1) @@ oam(8 * r + 2 * i)
    def setRow(r: Int, w0: UInt, from: Int) = {
      oam(8 * r) := w0(7 downto 0); oam(8 * r + 1) := w0(15 downto 8)
      if (from != r) for (i <- 2 until 8) oam(8 * r + i) := oam(8 * from + i)
    }
    when(hit) {
      switch(row) {
        for (r <- 1 until Gb.oamBytes / 8) is(r) {
          val (a, b, c) = (w(r, 0), w(r - 1, 0), w(r - 1, 2))
          // Read during an increase (rows 4 to 18): the row before is rewritten and copied to
          // two rows around it; the read corruption that follows leaves the rows as they are.
          if (r >= 4 && r < 19) when(read && increase) {
            val n = (b & (w(r - 2, 0) | c | a)) | (w(r - 2, 0) & a & c)
            for (t <- Seq(r - 2, r - 1, r)) setRow(t, n, r - 1)
          } otherwise {
            setRow(r, read ? (b | (a & c)) | (((a ^ c) & (b ^ c)) ^ c), r - 1)
          }
          else setRow(r, read ? (b | (a & c)) | (((a ^ c) & (b ^ c)) ^ c), r - 1)
        }
      }
    }
  }
}
