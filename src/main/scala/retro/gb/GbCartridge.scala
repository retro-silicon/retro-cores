package retro.gb

import retro.core._
import spinal.lib._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

/** The cartridge's memory bank controller. */
sealed trait Mbc
object Mbc {
  case object None extends Mbc
  case object Mbc1 extends Mbc
  case object Mbc5 extends Mbc
}

/** What a cartridge holds, read from its header (0x147 type, 0x148 ROM size, 0x149 RAM size). */
case class GbCartSpec(romBytes: Int, ramBytes: Int, mbc: Mbc) {
  require(isPow2(romBytes) && romBytes >= 0x8000, s"ROM size $romBytes")
  require(ramBytes == 0 || isPow2(ramBytes), s"RAM size $ramBytes")
}

object GbCartSpec {
  /** Supported types: 00 (ROM only), 01-03 (MBC1), 19-1E (MBC5), with optional RAM and battery. */
  def fromHeader(rom: Array[Byte]): GbCartSpec = {
    val kind = rom(0x147) & 0xFF
    val mbc = kind match {
      case 0x00 => Mbc.None
      case k if k <= 0x03 => Mbc.Mbc1
      case k if k >= 0x19 && k <= 0x1E => Mbc.Mbc5
      case k => throw new IllegalArgumentException(f"cartridge type $k%02X is not supported (ROM only, MBC1 and MBC5 are)")
    }
    // An image larger than its header says is taken whole, as emulators do (gbmicrotest
    // mbc1_rom_banks is a 2 MB image with a 32 KB header).
    val romBytes = (0x8000 << (rom(0x148) & 0xFF)) max (1 << log2Up(rom.length))
    val ramBytes = Seq(0, 0x800, 0x2000, 0x8000, 0x20000, 0x10000)(rom(0x149) & 0xFF)
    GbCartSpec(romBytes, if (mbc == Mbc.None) 0 else ramBytes, mbc)
  }
}

/** A cartridge on the Game Boy bus: ROM at 0000-7FFF, RAM at A000-BFFF and the MBC's bank
  * registers, written through the ROM range. ROM and RAM are `simPublic` memories, so a sim loads
  * an image with `load` instead of recompiling per ROM; on an FPGA they map to block RAM.
  *
  * MBC1: RAMG (0000-1FFF, 0x?A enables RAM), BANK1 (2000-3FFF, 5 bits, 0 reads as 1), BANK2
  * (4000-5FFF, 2 bits), MODE (6000-7FFF). BANK2 extends the ROM bank at 4000-7FFF and, in mode 1,
  * also selects the ROM bank at 0000-3FFF and the RAM bank. Disabled RAM reads 0xFF. Bank
  * numbers past the ROM size wrap, as the unconnected high address lines do.
  *
  * MBC5: RAMG (0000-1FFF, 0x0A enables RAM), ROMB (2000-2FFF low 8 bits, 3000-3FFF bit 8; bank 0
  * is allowed at 4000-7FFF), RAMB (4000-5FFF, 4 bits). 0000-3FFF is always bank 0. */
class GbCartridgePlugin(val spec: GbCartSpec) extends FiberPlugin {
  // The OAM DMA's reads, past the bus.
  private def romRead(address: UInt): UInt = logic.rom.readAsync(logic.romIndex(address))
  private def ramRead(address: UInt): UInt = if (spec.ramBytes == 0) U(0xFF, 8 bits) else logic.ram.read(address)

  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val romSlave = bus.newSlave(SizeMapping(0x0000, 0x8000), direct = Some(romRead))
    val ramSlave = bus.newSlave(SizeMapping(0xA000, 0x2000), direct = Some(ramRead))
    retainer.release()

    val romWidth = log2Up(spec.romBytes)
    val rom = Mem.fill(spec.romBytes)(UInt(8 bits)).setName("cart_rom").simPublic()

    val ramg = RegInit(False).allowUnsetRegToAvoidLatch()
    // MBC1: bank1 is BANK1, bank2 is BANK2. MBC5: bank1 is ROMB, bank2 is RAMB.
    val bank1 = Reg(UInt(9 bits)).init(1).allowUnsetRegToAvoidLatch()
    val bank2 = Reg(UInt(4 bits)).init(0).allowUnsetRegToAvoidLatch()
    val mode = RegInit(False).allowUnsetRegToAvoidLatch()
    val data = romSlave.cmd.data
    val a = romSlave.cmd.address
    when(romSlave.writeFire) {
      spec.mbc match {
        case Mbc.None =>
        case Mbc.Mbc1 =>
          switch(a(14 downto 13)) {
            is(0) { ramg := data(3 downto 0) === 0xA }
            is(1) { bank1 := ((data(4 downto 0) === 0) ? U(1, 5 bits) | data(4 downto 0)).resized }
            is(2) { bank2 := data(1 downto 0).resized }
            is(3) { mode := data(0) }
          }
        case Mbc.Mbc5 =>
          switch(a(14 downto 12)) {
            is(0, 1) { ramg := data === 0x0A }
            is(2) { bank1(7 downto 0) := data }
            is(3) { bank1(8) := data(0) }
            is(4, 5) { bank2 := data(3 downto 0) }
          }
      }
    }

    /** Where a bus address in 0000-7FFF lands in the ROM. */
    def romIndex(address: UInt): UInt = {
      val upper = address(14)
      val bank = UInt(9 bits)
      spec.mbc match {
        case Mbc.Mbc5 => bank := upper ? bank1 | U(0, 9 bits)
        case _ =>
          bank := (upper ? bank1(4 downto 0) | U(0, 5 bits)).resized
          when(upper || mode) { bank(6 downto 5) := bank2(1 downto 0) }
      }
      (bank @@ address(13 downto 0)).resize(romWidth)
    }
    romSlave.rdata := rom.readAsync(romIndex(romSlave.cmd.address))

    val ram = (spec.ramBytes > 0) generate new Area {
      val width = log2Up(spec.ramBytes)
      val mem = Mem.fill(spec.ramBytes)(UInt(8 bits)).setName("cart_ram").simPublic()
      val ramBank = spec.mbc match {
        case Mbc.Mbc5 => bank2
        case _ => mode ? bank2 | U(0, 4 bits)
      }
      def index(address: UInt) = (ramBank @@ address(12 downto 0)).resize(width)
      val enabled = if (spec.mbc == Mbc.None) True else ramg
      def read(address: UInt) = enabled ? mem.readAsync(index(address)) | U(0xFF, 8 bits)
      mem.write(index(ramSlave.cmd.address), ramSlave.cmd.data, ramSlave.writeFire && enabled)
      ramSlave.rdata := read(ramSlave.cmd.address)
    }
    if (spec.ramBytes == 0) ramSlave.rdata := 0xFF
  }

  /** Copies a ROM image into the simulated cartridge, and after the boot ROM its logo into VRAM. */
  def load(image: Array[Byte]): Unit = {
    require(image.length <= spec.romBytes, s"${image.length}-byte image, ${spec.romBytes}-byte ROM")
    val rom = logic.rom
    for (i <- 0 until spec.romBytes) rom.setBigInt(i, if (i < image.length) image(i) & 0xFF else 0xFF)
    for (p <- host.get[GbPostBootPlugin]) p.simLoadVram(a => if (a < image.length) image(a) & 0xFF else 0xFF)
  }
}
