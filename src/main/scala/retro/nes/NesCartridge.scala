package retro.nes

/** An iNES image: PRG ROM, CHR ROM (or 8 KB of CHR RAM when it has none), mapper and nametable
  * mirroring, plus the mapper's registers. Mappers supported: 0 (NROM), 1 (MMC1) and 3 (CNROM).
  * Like most emulators, every cartridge gets 8 KB of PRG RAM at 6000, which test ROMs use to
  * report results.
  *
  * MMC1 takes a register write 5 bits at a time, bit 0 first, through a shift register at
  * 8000-FFFF; address bits 13-14 of the fifth write pick the register (control, CHR bank 0, CHR
  * bank 1, PRG bank). A write with bit 7 set resets the shift register and sets PRG mode 3. A
  * write in the cycle after another one is ignored (the second write of a read-modify-write
  * instruction). PRG RAM is always enabled. */
case class NesCartridge(prg: Array[Int], chr: Array[Int], chrRam: Boolean, mapper: Int, verticalMirroring: Boolean) {
  require(mapper == 0 || mapper == 1 || mapper == 3, s"mapper $mapper is not supported")
  def prgMask = prg.length - 1
  def chrMask = chr.length - 1

  // ---- Mapper registers ----------------------------------------------------------------------
  /** CNROM's 8 KB CHR bank, or MMC1's CHR bank 0 (4 KB units). */
  var chrBank0 = 0
  var chrBank1, prgBank = 0
  /** MMC1: mirroring (bits 0-1), PRG mode (2-3) and CHR mode (4). */
  var control = 0x0C
  private var shift, shiftCount = 0
  private var lastWrite = -2L

  /** A CPU write to 8000-FFFF in CPU cycle `cycle`. */
  def cpuWrite(address: Int, value: Int, cycle: Long): Unit = mapper match {
    case 3 => chrBank0 = value & 3
    case 1 =>
      val ignored = cycle == lastWrite + 1
      lastWrite = cycle
      if ((value & 0x80) != 0) { shift = 0; shiftCount = 0; control |= 0x0C }
      else if (!ignored) {
        shift |= (value & 1) << shiftCount
        shiftCount += 1
        if (shiftCount == 5) {
          (address >> 13 & 3) match {
            case 0 => control = shift
            case 1 => chrBank0 = shift
            case 2 => chrBank1 = shift
            case 3 => prgBank = shift & 0x0F
          }
          shift = 0
          shiftCount = 0
        }
      }
    case _ =>
  }

  /** CPU address (8000-FFFF) to an index into PRG ROM. */
  def prgIndex(address: Int): Int = {
    val bank = if (mapper != 1) address >> 14 & 1 else (control >> 2 & 3) match {
      case 0 | 1 => (prgBank & 0x0E) | (address >> 14 & 1)
      case 2 => if ((address & 0x4000) == 0) 0 else prgBank
      case _ => if ((address & 0x4000) == 0) prgBank else 0x0F
    }
    // Bank numbers wrap at the number of banks, which need not be a power of 2 (80 KB images).
    (bank % (prg.length >> 14)) << 14 | address & 0x3FFF
  }
  /** PPU address (0000-1FFF) to an index into CHR. */
  def chrIndex(address: Int): Int = {
    val a = address & 0x1FFF
    val i = mapper match {
      case 3 => chrBank0 << 13 | a
      case 1 if (control & 0x10) == 0 => (chrBank0 & 0x1E) << 12 | a
      case 1 => (if (a < 0x1000) chrBank0 else chrBank1) << 12 | a & 0xFFF
      case _ => a
    }
    i & chrMask
  }
  /** Nametable address (2000-3FFF) to an index into the 2 KB of console VRAM. */
  def ciram(address: Int): Int = {
    val a = address & 0xFFF
    val mirroring = if (mapper == 1) control & 3 else if (verticalMirroring) 2 else 3
    mirroring match {
      case 0 => a & 0x3FF
      case 1 => 0x400 | a & 0x3FF
      case 2 => a & 0x7FF
      case _ => ((a >> 1) & 0x400) | (a & 0x3FF)
    }
  }
}

object NesCartridge {
  def load(image: Array[Int]): NesCartridge = {
    require(image.length >= 16 && image(0) == 'N' && image(1) == 'E' && image(2) == 'S' && image(3) == 0x1A, "not an iNES image")
    val prgSize = image(4) * 0x4000
    val chrSize = image(5) * 0x2000
    val trainer = if ((image(6) & 4) != 0) 512 else 0
    val prg = image.slice(16 + trainer, 16 + trainer + prgSize)
    val chr = if (chrSize == 0) Array.fill(0x2000)(0) else image.slice(16 + trainer + prgSize, 16 + trainer + prgSize + chrSize)
    NesCartridge(prg, chr, chrSize == 0, (image(6) >> 4) | (image(7) & 0xF0), (image(6) & 1) != 0)
  }
  def load(file: java.io.File): NesCartridge = load(java.nio.file.Files.readAllBytes(file.toPath).map(_ & 0xFF))
}
