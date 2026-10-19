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
  require(Seq(0, 1, 3, 4).contains(mapper), s"mapper $mapper is not supported")
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
  /** MMC3: bank select (8000), the eight bank registers R0-R7, mirroring (A000: 0 vertical),
    * and the scanline counter: latch (C000), reload request (C001), enable (E001; E000
    * disables and acknowledges). The counter is clocked by rises of PPU A12 after it was low for
    * `a12Filter` dots; on a clock it reloads when 0 or asked to, else decrements, then raises
    * the IRQ when it is 0 and enabled (the Sharp MMC3's behavior). */
  val mmc3Banks = Array(0, 2, 4, 5, 6, 7, 0, 1)
  var bankSelect, mmc3Mirroring = 0
  var irqLatch, irqCounter = 0
  var irqReload, irqEnabled = false
  /** The cartridge's IRQ output (MMC3). */
  var irq = false
  private var a12 = false
  private var a12Fell = 0L

  /** A CPU write to 8000-FFFF in CPU cycle `cycle`. */
  def cpuWrite(address: Int, value: Int, cycle: Long): Unit = mapper match {
    case 3 => chrBank0 = value & 3
    case 4 => (address & 0xE001) match {
      case 0x8000 => bankSelect = value
      case 0x8001 => mmc3Banks(bankSelect & 7) = value
      case 0xA000 => mmc3Mirroring = value & 1
      case 0xC000 => irqLatch = value
      case 0xC001 => irqCounter = 0; irqReload = true
      case 0xE000 => irqEnabled = false; irq = false
      case 0xE001 => irqEnabled = true
      case _ =>
    }
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

  /** The PPU drove `address` on its bus in PPU dot `dot` (counted from power-up). */
  def ppuBus(address: Int, dot: Long): Unit = if (mapper == 4) {
    val high = (address & 0x1000) != 0
    if (high && !a12 && dot - a12Fell >= NesCartridge.a12Filter) {
      if (irqCounter == 0 || irqReload) irqCounter = irqLatch else irqCounter -= 1
      if (irqCounter == 0 && irqEnabled) irq = true
      irqReload = false
    }
    if (!high && a12) a12Fell = dot
    a12 = high
  }

  /** CPU address (8000-FFFF) to an index into PRG ROM. */
  def prgIndex(address: Int): Int = if (mapper == 4) {
    val banks = prg.length >> 13
    val swap = (bankSelect & 0x40) != 0
    val bank = (address >> 13 & 3) match {
      case 0 => if (swap) banks - 2 else mmc3Banks(6)
      case 1 => mmc3Banks(7)
      case 2 => if (swap) mmc3Banks(6) else banks - 2
      case _ => banks - 1
    }
    (bank % banks) << 13 | address & 0x1FFF
  } else {
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
      case 4 =>
        val slot = (a >> 10) ^ (if ((bankSelect & 0x80) != 0) 4 else 0)
        val bank = if (slot < 4) (mmc3Banks(slot >> 1) & 0xFE) | (slot & 1) else mmc3Banks(slot - 2)
        bank << 10 | a & 0x3FF
      case _ => a
    }
    i & chrMask
  }
  /** Nametable address (2000-3FFF) to an index into the 2 KB of console VRAM. */
  def ciram(address: Int): Int = {
    val a = address & 0xFFF
    val mirroring = if (mapper == 1) control & 3 else if (mapper == 4) 2 + mmc3Mirroring else if (verticalMirroring) 2 else 3
    mirroring match {
      case 0 => a & 0x3FF
      case 1 => 0x400 | a & 0x3FF
      case 2 => a & 0x7FF
      case _ => ((a >> 1) & 0x400) | (a & 0x3FF)
    }
  }
}

object NesCartridge {
  /** PPU dots A12 must stay low before a rise clocks MMC3's counter. */
  val a12Filter = 10
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
