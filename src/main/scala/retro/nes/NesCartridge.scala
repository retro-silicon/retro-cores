package retro.nes

/** An iNES image: PRG ROM, CHR ROM (or CHR RAM when it has none), mapper, nametable mirroring
  * and PRG RAM size, with its `board` (one `NesBoard` class per mapper). Like most emulators,
  * every cartridge gets at least 8 KB of PRG RAM at 6000, which test ROMs use to report results
  * (more when an NES 2.0 header asks: SXROM's 32 KB). */
case class NesCartridge(prg: Array[Int], chr: Array[Int], chrRam: Boolean, mapper: Int, verticalMirroring: Boolean,
                        mmc3Alternate: Boolean = false, prgRamBytes: Int = 0x2000) {
  require(NesCartridge.mappers.contains(mapper), s"mapper $mapper is not supported")
  def prgMask = prg.length - 1
  def chrMask = chr.length - 1
  /** The mapper: its registers and how they map addresses. */
  val board: NesBoard = NesBoard(this)

  /** A CPU write to 8000-FFFF in CPU cycle `cycle`. */
  def cpuWrite(address: Int, value: Int, cycle: Long): Unit = board.cpuWrite(address, value, cycle)
  /** A CPU cycle starts (before its access). */
  def cpuCycle(): Unit = board.cpuCycle()
  /** The PPU drove `address` on its bus in PPU dot `dot` (counted from power-up). */
  def ppuBus(address: Int, dot: Long): Unit = board.ppuBus(address, dot)
  /** The cartridge's IRQ output. */
  def irq: Boolean = board.irq
  /** CPU address (8000-FFFF) to an index into PRG ROM. */
  def prgIndex(address: Int): Int = board.prgIndex(address)
  /** PPU address (0000-1FFF) to an index into CHR. */
  def chrIndex(address: Int): Int = board.chrIndex(address & 0x1FFF) & chrMask
  /** Nametable address (2000-3FFF) to an index into the 2 KB of console VRAM. */
  def ciram(address: Int): Int = {
    val a = address & 0xFFF
    board.mirroring match {
      case 0 => a & 0x3FF
      case 1 => 0x400 | a & 0x3FF
      case 2 => a & 0x7FF
      case _ => ((a >> 1) & 0x400) | (a & 0x3FF)
    }
  }
  private def ramIndex(address: Int) = (board.ramBank % (prgRamBytes >> 13)) << 13 | address & 0x1FFF
  /** A CPU read of 6000-7FFF, where most boards have PRG RAM (`ram`, `prgRamBytes` of it). */
  def lowRead(address: Int, ram: Array[Int], openBus: Int): Int =
    if (board.ramReadable) ram(ramIndex(address)) else board.lowOff(address, openBus)
  /** A CPU write to 6000-7FFF. */
  def lowWrite(address: Int, value: Int, ram: Array[Int]): Unit =
    if (board.ramWritable) ram(ramIndex(address)) = value
}

/** A cartridge board: the mapper's registers and how they map the CPU's and the PPU's addresses.
  * As is, NROM (0): 16 or 32 KB of PRG, 8 KB of CHR, mirroring from the header. The other
  * boards override what they change. */
class NesBoard(val cart: NesCartridge) {
  def banks8 = cart.prg.length >> 13
  def banks16 = cart.prg.length >> 14
  /** PRG index of `address` in an 8, 16 or 32 KB bank; bank numbers wrap at the number of
    * banks, which need not be a power of 2 (80 KB MMC1 images). */
  protected def prg8(bank: Int, address: Int) = (bank % banks8) << 13 | address & 0x1FFF
  protected def prg16(bank: Int, address: Int) = (bank % banks16) << 14 | address & 0x3FFF
  protected def prg32(bank: Int, address: Int) = prg16(bank << 1 | address >> 14 & 1, address)

  def cpuWrite(address: Int, value: Int, cycle: Long): Unit = {}
  def cpuCycle(): Unit = {}
  def ppuBus(address: Int, dot: Long): Unit = {}
  def irq = false
  def prgIndex(address: Int): Int = prg16(address >> 14 & 1, address)
  /** PPU address (0000-1FFF) to an index into CHR, before it wraps at the CHR size. */
  def chrIndex(a: Int): Int = a
  /** Nametable mirroring: 0 and 1 one-screen (low, high), 2 vertical, 3 horizontal. */
  def mirroring: Int = if (cart.verticalMirroring) 2 else 3
  /** Whether 6000-7FFF reads and writes PRG RAM, and which 8 KB bank of it. */
  def ramReadable = true
  def ramWritable = ramReadable
  def ramBank = 0
  /** A read of 6000-7FFF with the RAM disabled. */
  def lowOff(address: Int, openBus: Int): Int = openBus
}

object NesBoard {
  def apply(cart: NesCartridge): NesBoard = cart.mapper match {
    case 0 => new NesBoard(cart)
    case 1 => new Mmc1(cart)
    case 2 => new Uxrom(cart)
    case 3 => new Cnrom(cart)
    case 4 => new Mmc3(cart)
    case 7 => new Axrom(cart)
    case 9 => new Mmc2(cart, mmc4 = false)
    case 10 => new Mmc2(cart, mmc4 = true)
    case 11 => new ColorDreams(cart)
    case 34 => new Bnrom(cart)
    case 66 => new Gxrom(cart)
    case 69 => new Fme7(cart)
    case 180 => new Mapper180(cart)
  }
}

/** MMC1 (1) takes a register write 5 bits at a time, bit 0 first, through a shift register at
  * 8000-FFFF; address bits 13-14 of the fifth write pick the register (control, CHR bank 0, CHR
  * bank 1, PRG bank). A write with bit 7 set resets the shift register and sets PRG mode 3. A
  * write in the cycle after another one is ignored (the second write of a read-modify-write
  * instruction). PRG RAM is disabled by PRG bit 4 (MMC1B), and on SNROM (CHR RAM, at most 256 KB
  * of PRG) also by CHR A16; SXROM (32 KB of PRG RAM) banks it by CHR bits 2-3, and SUROM and
  * SXROM (512 KB of PRG) take PRG A18 from CHR bit 4. */
class Mmc1(cart: NesCartridge) extends NesBoard(cart) {
  /** Mirroring (bits 0-1), PRG mode (2-3) and CHR mode (4). */
  var control = 0x0C
  var chrBank0, chrBank1, prgBank = 0
  private var shift, shiftCount = 0
  private var lastWrite = -2L
  /** PRG RAM disabled, and whether C000 was the CHR register written last (in 4 KB mode its
    * bits then drive PRG A18 and the PRG RAM lines, as in Mesen). */
  var ramOff, lastChr1 = false
  /** The CHR register whose bits 2-4 drive PRG A18 and the PRG RAM lines. */
  private def extra = if (lastChr1 && (control & 0x10) != 0) chrBank1 else chrBank0

  override def cpuWrite(address: Int, value: Int, cycle: Long): Unit = {
    val ignored = cycle == lastWrite + 1
    lastWrite = cycle
    if ((value & 0x80) != 0) { shift = 0; shiftCount = 0; control |= 0x0C }
    else if (!ignored) {
      shift |= (value & 1) << shiftCount
      shiftCount += 1
      if (shiftCount == 5) {
        (address >> 13 & 3) match {
          case 0 => control = shift
          case 1 => chrBank0 = shift; lastChr1 = false
          case 2 => chrBank1 = shift; lastChr1 = true
          case 3 => prgBank = shift & 0x0F; ramOff = (shift & 0x10) != 0
        }
        shift = 0
        shiftCount = 0
      }
    }
  }
  override def prgIndex(address: Int): Int = {
    val bank = (control >> 2 & 3) match {
      case 0 | 1 => (prgBank & 0x0E) | (address >> 14 & 1)
      case 2 => if ((address & 0x4000) == 0) 0 else prgBank
      case _ => if ((address & 0x4000) == 0) prgBank else 0x0F
    }
    val outer = if (cart.prg.length == 0x80000) extra & 0x10 else 0
    prg16(bank | outer, address)
  }
  override def chrIndex(a: Int): Int =
    if ((control & 0x10) == 0) (chrBank0 & 0x1E) << 12 | a
    else (if (a < 0x1000) chrBank0 else chrBank1) << 12 | a & 0xFFF
  override def mirroring = control & 3
  override def ramReadable = !ramOff && !(cart.chrRam && cart.prg.length <= 0x40000 && (extra & 0x10) != 0)
  override def ramBank = extra >> 2 & 3
}

/** CNROM (3): an 8 KB CHR bank, written anywhere in 8000-FFFF. */
class Cnrom(cart: NesCartridge) extends NesBoard(cart) {
  var chrBank = 0
  override def cpuWrite(address: Int, value: Int, cycle: Long): Unit = chrBank = value & 3
  override def chrIndex(a: Int): Int = chrBank << 13 | a
}

/** The discrete-logic boards: one register, written anywhere in 8000-FFFF (no bus conflicts
  * modelled). */
abstract class LatchBoard(cart: NesCartridge) extends NesBoard(cart) {
  var latch = 0
  override def cpuWrite(address: Int, value: Int, cycle: Long): Unit = latch = value
}
/** UxROM (2): a 16 KB bank at 8000, the last bank at C000. */
class Uxrom(cart: NesCartridge) extends LatchBoard(cart) {
  override def prgIndex(address: Int): Int = prg16(if ((address & 0x4000) == 0) latch else banks16 - 1, address)
}
/** Mapper 180, UxROM reversed: the first bank at 8000, the selected one at C000. */
class Mapper180(cart: NesCartridge) extends LatchBoard(cart) {
  override def prgIndex(address: Int): Int = prg16(if ((address & 0x4000) == 0) 0 else latch, address)
}
/** AxROM (7): a 32 KB bank in bits 0-3, one-screen mirroring from bit 4. */
class Axrom(cart: NesCartridge) extends LatchBoard(cart) {
  override def prgIndex(address: Int): Int = prg32(latch & 0x0F, address)
  override def mirroring = latch >> 4 & 1
}
/** Color Dreams (11): a 32 KB bank in bits 0-1, an 8 KB CHR bank in bits 4-7. */
class ColorDreams(cart: NesCartridge) extends LatchBoard(cart) {
  override def prgIndex(address: Int): Int = prg32(latch & 3, address)
  override def chrIndex(a: Int): Int = (latch >> 4) << 13 | a
}
/** BNROM (34): a 32 KB bank. */
class Bnrom(cart: NesCartridge) extends LatchBoard(cart) {
  override def prgIndex(address: Int): Int = prg32(latch, address)
}
/** GxROM (66): a 32 KB bank in bits 4-5, an 8 KB CHR bank in bits 0-1. */
class Gxrom(cart: NesCartridge) extends LatchBoard(cart) {
  override def prgIndex(address: Int): Int = prg32(latch >> 4 & 3, address)
  override def chrIndex(a: Int): Int = (latch & 3) << 13 | a
}

/** MMC3 (4): bank select (8000), the eight bank registers R0-R7, mirroring (A000: 0
  * vertical), PRG RAM enable and write protect (A001 bits 7 and 6; enabled at power-up here), and
  * the scanline counter: latch (C000), reload request (C001), enable (E001; E000 disables and
  * acknowledges). The counter is clocked by rises of PPU A12 after it was low for `a12Filter`
  * dots; on a clock it reloads when 0 or asked to, else decrements, then raises the IRQ when it
  * is 0 and enabled (the Sharp MMC3's behavior). With `mmc3Alternate` (the MMC3 revision A, NEC's
  * MMC3 and MMC6) the IRQ is raised only when the counter got to 0 by decrementing or by a
  * reload after C001, not by reloading 0 when it was already 0. */
class Mmc3(cart: NesCartridge) extends NesBoard(cart) {
  val banks = Array(0, 2, 4, 5, 6, 7, 0, 1)
  var bankSelect, horizontal = 0
  var ramOn = true
  var ramProtect = false
  var irqLatch, irqCounter = 0
  var irqReload, irqEnabled = false
  private var irqOut = false
  private var a12 = false
  private var a12Fell = 0L

  override def cpuWrite(address: Int, value: Int, cycle: Long): Unit = (address & 0xE001) match {
    case 0x8000 => bankSelect = value
    case 0x8001 => banks(bankSelect & 7) = value
    case 0xA000 => horizontal = value & 1
    case 0xA001 => ramOn = (value & 0x80) != 0; ramProtect = (value & 0x40) != 0
    case 0xC000 => irqLatch = value
    case 0xC001 => irqCounter = 0; irqReload = true
    case 0xE000 => irqEnabled = false; irqOut = false
    case 0xE001 => irqEnabled = true
    case _ =>
  }
  override def ppuBus(address: Int, dot: Long): Unit = {
    val high = (address & 0x1000) != 0
    if (high && !a12 && dot - a12Fell >= NesCartridge.a12Filter) {
      val raise = !cart.mmc3Alternate || irqCounter != 0 || irqReload
      if (irqCounter == 0 || irqReload) irqCounter = irqLatch else irqCounter -= 1
      if (irqCounter == 0 && irqEnabled && raise) irqOut = true
      irqReload = false
    }
    if (!high && a12) a12Fell = dot
    a12 = high
  }
  override def irq = irqOut
  override def prgIndex(address: Int): Int = {
    val swap = (bankSelect & 0x40) != 0
    val bank = (address >> 13 & 3) match {
      case 0 => if (swap) banks8 - 2 else banks(6)
      case 1 => banks(7)
      case 2 => if (swap) banks(6) else banks8 - 2
      case _ => banks8 - 1
    }
    prg8(bank, address)
  }
  override def chrIndex(a: Int): Int = {
    val slot = (a >> 10) ^ (if ((bankSelect & 0x80) != 0) 4 else 0)
    val bank = if (slot < 4) (banks(slot >> 1) & 0xFE) | (slot & 1) else banks(slot - 2)
    bank << 10 | a & 0x3FF
  }
  override def mirroring = 2 + horizontal
  override def ramReadable = ramOn
  override def ramWritable = ramOn && !ramProtect
}

/** MMC2 (9) and MMC4 (10): an 8 KB (MMC2) or 16 KB (MMC4) PRG bank at 8000 (A000-AFFF) with the
  * last banks fixed after it, two 4 KB CHR banks for each pattern table (B000-EFFF), picked by
  * two latches the PPU's fetches set: fetching 0FD8 or 0FE8 sets the low table's to FD or FE,
  * 1FD8-1FDF or 1FE8-1FEF the high table's (MMC4: 0FD8-0FDF and 0FE8-0FEF too). As in Mesen, a
  * latch changes at the PPU's next bus address, so the fetch that set it still reads the old
  * bank. F000: mirroring (0 vertical). */
class Mmc2(cart: NesCartridge, mmc4: Boolean) extends NesBoard(cart) {
  var prgBank, horizontal = 0
  /** The CHR banks for latch values FD and FE, low table then high table; the latches (true:
    * FE) and the latch change waiting for the next bus address. */
  val banks = Array(0, 0, 0, 0)
  val latch = Array(true, true)
  private var pending = -1

  override def cpuWrite(address: Int, value: Int, cycle: Long): Unit = (address >> 12) match {
    case 0xA => prgBank = value & 0x0F
    case 0xB | 0xC | 0xD | 0xE => banks((address >> 12) - 0xB) = value & 0x1F
    case 0xF => horizontal = value & 1
    case _ =>
  }
  override def ppuBus(address: Int, dot: Long): Unit = {
    if (pending >= 0) { latch(pending >> 1) = (pending & 1) != 0; pending = -1 }
    val a = address & 0x3FFF
    val exact = !mmc4 && a < 0x1000
    if (a < 0x2000 && (if (exact) a == 0x0FD8 else (a & 0x0FF8) == 0x0FD8)) pending = (a >> 12) << 1
    if (a < 0x2000 && (if (exact) a == 0x0FE8 else (a & 0x0FF8) == 0x0FE8)) pending = (a >> 12) << 1 | 1
  }
  override def prgIndex(address: Int): Int =
    if (mmc4) prg16(if ((address & 0x4000) == 0) prgBank else banks16 - 1, address)
    else prg8(if ((address & 0x6000) == 0) prgBank else banks8 - 4 + (address >> 13 & 3), address)
  override def chrIndex(a: Int): Int = {
    val table = a >> 12
    banks(table * 2 + (if (latch(table)) 1 else 0)) << 12 | a & 0xFFF
  }
  override def mirroring = 2 + horizontal
}

/** Sunsoft FME-7 (69): a command register (8000-9FFF) picks what the parameter register
  * (A000-BFFF) sets: 0-7 the 1 KB CHR banks, 8 what 6000-7FFF shows (bits 0-5 a ROM bank, bit 6
  * RAM instead, bit 7 the RAM enabled; disabled RAM reads open bus), 9-B the 8 KB PRG banks at
  * 8000, A000 and C000 (E000 is the last bank), C mirroring (vertical, horizontal, one-screen
  * low, high), D the IRQ (bit 0 enabled, bit 7 counting; writing acknowledges), E-F the 16-bit
  * counter, which counts down every CPU cycle before the access and raises the IRQ when it wraps
  * from 0. The 5B sound chip is not modelled. */
class Fme7(cart: NesCartridge) extends NesBoard(cart) {
  var command = 0
  /** The 16 registers the command addresses (13-15 unused). */
  val regs = Array.fill(16)(0)
  var counter = 0
  private var irqOut = false

  override def cpuWrite(address: Int, value: Int, cycle: Long): Unit = (address & 0xE000) match {
    case 0x8000 => command = value & 0x0F
    case 0xA000 =>
      regs(command) = value
      command match {
        case 0xD => irqOut = false
        case 0xE => counter = counter & 0xFF00 | value
        case 0xF => counter = counter & 0x00FF | value << 8
        case _ =>
      }
    case _ =>
  }
  override def cpuCycle(): Unit = if ((regs(0xD) & 0x80) != 0) {
    counter = (counter - 1) & 0xFFFF
    if (counter == 0xFFFF && (regs(0xD) & 1) != 0) irqOut = true
  }
  override def irq = irqOut
  override def prgIndex(address: Int): Int = {
    val slot = address >> 13 & 3
    prg8(if (slot == 3) banks8 - 1 else regs(9 + slot) & 0x3F, address)
  }
  override def chrIndex(a: Int): Int = regs(a >> 10) << 10 | a & 0x3FF
  override def mirroring = Seq(2, 3, 0, 1)(regs(0xC) & 3)
  override def ramReadable = (regs(8) & 0xC0) == 0xC0
  override def lowOff(address: Int, openBus: Int): Int =
    if ((regs(8) & 0x40) != 0) openBus else cart.prg(prg8(regs(8) & 0x3F, address))
}

object NesCartridge {
  val mappers = Seq(0, 1, 2, 3, 4, 7, 9, 10, 11, 34, 66, 69, 180)
  /** PPU dots A12 must stay low before a rise clocks MMC3's counter. */
  val a12Filter = 10
  def load(image: Array[Int]): NesCartridge = {
    require(image.length >= 16 && image(0) == 'N' && image(1) == 'E' && image(2) == 'S' && image(3) == 0x1A, "not an iNES image")
    val prgSize = image(4) * 0x4000
    val chrSize = image(5) * 0x2000
    val trainer = if ((image(6) & 4) != 0) 512 else 0
    val prg = image.slice(16 + trainer, 16 + trainer + prgSize)
    // NES 2.0 headers give the CHR RAM size (64 << n bytes); iNES ones mean 8 KB.
    val nes2 = (image(7) & 0x0C) == 0x08
    val chrRamSize = if (nes2 && (image(11) & 0x0F) != 0) 64 << (image(11) & 0x0F) else 0x2000
    val chr = if (chrSize == 0) Array.fill(chrRamSize)(0) else image.slice(16 + trainer + prgSize, 16 + trainer + prgSize + chrSize)
    // NES 2.0: PRG RAM plus PRG NVRAM (64 << n bytes each); at least the 8 KB every board gets here.
    def shifted(n: Int) = if (n == 0) 0 else 64 << n
    val prgRam = if (nes2) shifted(image(10) & 0x0F) + shifted(image(10) >> 4) else 0
    NesCartridge(prg, chr, chrSize == 0, (image(6) >> 4) | (image(7) & 0xF0), (image(6) & 1) != 0,
      prgRamBytes = prgRam max 0x2000)
  }
  def load(file: java.io.File): NesCartridge = load(java.nio.file.Files.readAllBytes(file.toPath).map(_ & 0xFF))
}
