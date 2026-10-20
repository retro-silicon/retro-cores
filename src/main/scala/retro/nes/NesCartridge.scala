package retro.nes

/** An iNES image: PRG ROM, CHR ROM (or 8 KB of CHR RAM when it has none), mapper and nametable
  * mirroring, plus the mapper's registers. Mappers supported: 0 (NROM), 1 (MMC1), 3 (CNROM), 4
  * (MMC3), and the discrete-logic boards whose one register is written anywhere in 8000-FFFF
  * (`discrete`): 2 (UxROM: 16 KB bank at 8000, last bank at C000), 180 (the reverse: first bank
  * at 8000, the selected one at C000), 7 (AxROM: 32 KB bank in bits 0-3, one-screen mirroring
  * from bit 4), 34 (BNROM: 32 KB bank), 66 (GxROM: 32 KB bank in bits 4-5, 8 KB CHR bank in
  * bits 0-1) and 11 (Color Dreams: 32 KB bank in bits 0-1, 8 KB CHR bank in bits 4-7). None of
  * them models bus conflicts.
  *
  * MMC2 (9) and MMC4 (10): an 8 KB (MMC2) or 16 KB (MMC4) PRG bank at 8000 (A000-AFFF) with the
  * last banks fixed after it, two 4 KB CHR banks for each pattern table (B000-EFFF), picked by
  * two latches the PPU's fetches set: fetching 0FD8 or 0FE8 sets the low table's to FD or FE,
  * 1FD8-1FDF or 1FE8-1FEF the high table's (MMC4: 0FD8-0FDF and 0FE8-0FEF too). As in Mesen, a
  * latch changes at the PPU's next bus address, so the fetch that set it still reads the old
  * bank. F000: mirroring (0 vertical).
  *
  * Sunsoft FME-7 (69): a command register (8000-9FFF) picks what the parameter register
  * (A000-BFFF) sets: 0-7 the 1 KB CHR banks, 8 what 6000-7FFF shows (bits 0-5 a ROM bank, bit 6
  * RAM instead, bit 7 the RAM enabled; disabled RAM reads open bus), 9-B the 8 KB PRG banks at
  * 8000, A000 and C000 (E000 is the last bank), C mirroring (vertical, horizontal, one-screen
  * low, high), D the IRQ (bit 0 enabled, bit 7 counting; writing acknowledges), E-F the 16-bit
  * counter, which counts down every CPU cycle before the access and raises the IRQ when it wraps
  * from 0. The 5B sound chip is not modelled.
  * Like most emulators, every cartridge gets 8 KB of PRG RAM at 6000, which test ROMs use to
  * report results.
  *
  * MMC1 takes a register write 5 bits at a time, bit 0 first, through a shift register at
  * 8000-FFFF; address bits 13-14 of the fifth write pick the register (control, CHR bank 0, CHR
  * bank 1, PRG bank). A write with bit 7 set resets the shift register and sets PRG mode 3. A
  * write in the cycle after another one is ignored (the second write of a read-modify-write
  * instruction). PRG RAM is always enabled. */
case class NesCartridge(prg: Array[Int], chr: Array[Int], chrRam: Boolean, mapper: Int, verticalMirroring: Boolean) {
  require(NesCartridge.mappers.contains(mapper), s"mapper $mapper is not supported")
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

  /** The discrete boards' register. */
  var latch = 0
  /** MMC2/MMC4: the CHR banks for latch values FD and FE, low table then high table, the
    * latches (true: FE) and the latch change waiting for the next bus address. */
  val mmc2Banks = Array(0, 0, 0, 0)
  val mmc2Latch = Array(true, true)
  private var mmc2Pending = -1
  /** FME-7: the command, the 16 registers it addresses (13-15 unused), the counter. */
  var fme7Command = 0
  val fme7 = Array.fill(16)(0)
  var fme7Counter = 0

  /** A CPU write to 8000-FFFF in CPU cycle `cycle`. */
  def cpuWrite(address: Int, value: Int, cycle: Long): Unit = mapper match {
    case 3 => chrBank0 = value & 3
    case m if NesCartridge.discrete.contains(m) => latch = value
    case 69 => (address & 0xE000) match {
      case 0x8000 => fme7Command = value & 0x0F
      case 0xA000 =>
        fme7(fme7Command) = value
        fme7Command match {
          case 0xD => irq = false
          case 0xE => fme7Counter = fme7Counter & 0xFF00 | value
          case 0xF => fme7Counter = fme7Counter & 0x00FF | value << 8
          case _ =>
        }
      case _ =>
    }
    case 9 | 10 => (address >> 12) match {
      case 0xA => prgBank = value & 0x0F
      case 0xB | 0xC | 0xD | 0xE => mmc2Banks((address >> 12) - 0xB) = value & 0x1F
      case 0xF => mmc3Mirroring = value & 1
      case _ =>
    }
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

  /** A CPU cycle starts (before its access). */
  def cpuCycle(): Unit = if (mapper == 69 && (fme7(0xD) & 0x80) != 0) {
    fme7Counter = (fme7Counter - 1) & 0xFFFF
    if (fme7Counter == 0xFFFF && (fme7(0xD) & 1) != 0) irq = true
  }
  /** A CPU read of 6000-7FFF, where most boards have the 8 KB of `ram`. */
  def lowRead(address: Int, ram: Array[Int], openBus: Int): Int =
    if (mapper != 69 || (fme7(8) & 0xC0) == 0xC0) ram(address & 0x1FFF)
    else if ((fme7(8) & 0x40) != 0) openBus
    else prg(((fme7(8) & 0x3F) % (prg.length >> 13)) << 13 | address & 0x1FFF)
  /** A CPU write to 6000-7FFF. */
  def lowWrite(address: Int, value: Int, ram: Array[Int]): Unit =
    if (mapper != 69 || (fme7(8) & 0xC0) == 0xC0) ram(address & 0x1FFF) = value

  /** The PPU drove `address` on its bus in PPU dot `dot` (counted from power-up). */
  def ppuBus(address: Int, dot: Long): Unit = if (mapper == 9 || mapper == 10) {
    if (mmc2Pending >= 0) { mmc2Latch(mmc2Pending >> 1) = (mmc2Pending & 1) != 0; mmc2Pending = -1 }
    val a = address & 0x3FFF
    val exact = mapper == 9 && a < 0x1000
    if (a < 0x2000 && (if (exact) a == 0x0FD8 else (a & 0x0FF8) == 0x0FD8)) mmc2Pending = (a >> 12) << 1
    if (a < 0x2000 && (if (exact) a == 0x0FE8 else (a & 0x0FF8) == 0x0FE8)) mmc2Pending = (a >> 12) << 1 | 1
  } else if (mapper == 4) {
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
  } else if (mapper == 69) {
    val banks = prg.length >> 13
    val slot = address >> 13 & 3
    val bank = if (slot == 3) banks - 1 else fme7(9 + slot) & 0x3F
    (bank % banks) << 13 | address & 0x1FFF
  } else if (mapper == 9) {
    val banks = prg.length >> 13
    val bank = if ((address & 0x6000) == 0) prgBank else banks - 4 + (address >> 13 & 3)
    (bank % banks) << 13 | address & 0x1FFF
  } else if (mapper == 10) {
    val banks = prg.length >> 14
    val bank = if ((address & 0x4000) == 0) prgBank else banks - 1
    (bank % banks) << 14 | address & 0x3FFF
  } else if (NesCartridge.discrete.contains(mapper)) {
    val banks16 = prg.length >> 14
    val bank16 = mapper match {
      case 2 => if ((address & 0x4000) == 0) latch else banks16 - 1
      case 180 => if ((address & 0x4000) == 0) 0 else latch
      case _ =>
        val bank32 = mapper match { case 7 => latch & 0x0F; case 34 => latch; case 66 => latch >> 4 & 3; case _ => latch & 3 }
        bank32 << 1 | address >> 14 & 1
    }
    (bank16 % banks16) << 14 | address & 0x3FFF
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
      case 66 => (latch & 3) << 13 | a
      case 69 => fme7(a >> 10) << 10 | a & 0x3FF
      case 9 | 10 =>
        val table = a >> 12
        mmc2Banks(table * 2 + (if (mmc2Latch(table)) 1 else 0)) << 12 | a & 0xFFF
      case 11 => (latch >> 4) << 13 | a
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
    val mirroring = if (mapper == 1) control & 3 else if (mapper == 4 || mapper == 9 || mapper == 10) 2 + mmc3Mirroring
      else if (mapper == 7) latch >> 4 & 1 else if (mapper == 69) Seq(2, 3, 0, 1)(fme7(0xC) & 3)
      else if (verticalMirroring) 2 else 3
    mirroring match {
      case 0 => a & 0x3FF
      case 1 => 0x400 | a & 0x3FF
      case 2 => a & 0x7FF
      case _ => ((a >> 1) & 0x400) | (a & 0x3FF)
    }
  }
}

object NesCartridge {
  /** The boards with one register written anywhere in 8000-FFFF, besides CNROM. */
  val discrete = Seq(2, 7, 11, 34, 66, 180)
  val mappers = Seq(0, 1, 3, 4, 9, 10, 69) ++ discrete
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
