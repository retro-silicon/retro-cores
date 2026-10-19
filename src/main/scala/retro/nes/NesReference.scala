package retro.nes

import retro.r2a03.{R2A03Bus, R2A03Reference}

/** NES system model: `R2A03Reference` as the CPU, 2 KB of RAM, `NesPpuReference`, OAM DMA, the
  * APU frame counter's IRQ, the controller ports and the cartridge. One `step()` is one CPU
  * instruction; the bus advances the rest of the machine cycle by cycle.
  *
  * Timing rules, one CPU cycle at a time: the PPU runs `dotsBefore` dots, the CPU access
  * happens, the PPU runs the remaining dots, then the APU frame counter advances and the CPU
  * samples its NMI and IRQ lines.
  * These are what ppu_vbl_nmi pins down: a 2002 read one dot before VBlank suppresses the flag,
  * one or two dots after it suppresses the NMI, and NMI timing to the PPU dot.
  *
  * OAM DMA (a write to 4014) halts the CPU on its next read: one halt cycle, one more to align
  * when it would start on an odd cycle, then 256 get/put pairs. The halted read is repeated
  * in those halt and alignment cycles, side effects included, as on the real chip. */
class NesReference(val cart: NesCartridge, val dotsBefore: Int = 2) {
  val ram = Array.fill(0x800)(0)
  val prgRam = Array.fill(0x2000)(0)
  val ppu = new NesPpuReference(cart)
  var openBus = 0
  /** CPU cycles since power-on. */
  var cycles = 0L
  /** Controller 1 buttons (A B Select Start Up Down Left Right in bits 0-7). */
  var buttons = 0
  private var strobe = false
  private var shift = 0

  // ---- APU: frame counter, its IRQ and the four length counters ------------------------------
  /** Length counters of pulse 1, pulse 2, triangle and noise (no sound is generated). */
  val length = Array.fill(4)(0)
  private val lengthHalt = Array.fill(4)(false)
  private var lengthEnabled = 0
  private val lengthTable = Array(10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
                                  12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30)
  private def halfFrame(): Unit = for (i <- 0 until 4) if (length(i) > 0 && !lengthHalt(i)) length(i) -= 1

  var frameIrq = false
  private var frameCounter = 0
  private var fiveStep, irqInhibit = false
  private var counterReset = 0
  /** One APU clock, ahead of the CPU access in the same cycle (so a 4015 read sees the flag
    * set in its own cycle). A 4017 write resets the counter 3 or 4 cycles later, by the parity
    * of the write cycle; the IRQ flag is then raised 29828-29830 cycles after the reset. */
  private def apuCycle(): Unit = {
    frameCounter += 1
    if (counterReset > 0) {
      counterReset -= 1
      if (counterReset == 0) { frameCounter = 0; if (fiveStep) halfFrame() }
    }
    if (!fiveStep) {
      if (frameCounter == 14913 || frameCounter == 29829) halfFrame()
      if (frameCounter >= 29828 && frameCounter <= 29830 && !irqInhibit) frameIrq = true
      if (frameCounter == 29830) frameCounter = 0
    } else {
      if (frameCounter == 14913 || frameCounter == 37281) halfFrame()
      if (frameCounter == 37282) frameCounter = 0
    }
  }

  private var dmaPage: Option[Int] = None

  val bus: R2A03Bus = new R2A03Bus {
    def read(address: Int) = {
      for (page <- dmaPage) { dmaPage = None; dma(page, address) }
      cycle(load(address))
    }
    def write(address: Int, value: Int) = cycle({ store(address, value); value })
  }
  val cpu = new R2A03Reference(bus, _ => (ppu.nmi, frameIrq))

  /** One CPU cycle around `access`. */
  private def cycle(access: => Int): Int = {
    for (_ <- 0 until dotsBefore) ppu.tick()
    apuCycle()
    val v = access
    openBus = v
    for (_ <- dotsBefore until 3) ppu.tick()
    cycles += 1
    v
  }
  private def dma(page: Int, haltedAt: Int): Unit = {
    def stolen(access: => Int) = { cycle(access); cpu.haltedCycle() }
    stolen(load(haltedAt))
    if ((cycles & 1) == 0) stolen(load(haltedAt))
    for (i <- 0 until 256) {
      var v = 0
      stolen({ v = load(page << 8 | i); v })
      stolen({ ppu.cpuWrite(4, v); v })
    }
  }

  def load(address: Int): Int = address match {
    case a if a < 0x2000 => ram(a & 0x7FF)
    case a if a < 0x4000 => ppu.cpuRead(a)
    case 0x4015 =>
      val r = (if (frameIrq) 0x40 else 0) | (openBus & 0x20) | (0 until 4).map(i => if (length(i) > 0) 1 << i else 0).sum
      frameIrq = false
      r
    case 0x4016 =>
      val bit = if (strobe) buttons & 1 else { val b = shift & 1; shift = (shift >> 1) | 0x80; b }
      (openBus & 0xE0) | bit
    case a if a < 0x6000 => openBus
    case a if a < 0x8000 => prgRam(a & 0x1FFF)
    case a => cart.prg(a & cart.prgMask)
  }
  def store(address: Int, value: Int): Unit = address match {
    case a if a < 0x2000 => ram(a & 0x7FF) = value
    case a if a < 0x4000 => ppu.cpuWrite(a, value)
    case 0x4000 | 0x4004 | 0x400C => lengthHalt(address >> 2 & 3) = (value & 0x20) != 0
    case 0x4008 => lengthHalt(2) = (value & 0x80) != 0
    case 0x4003 | 0x4007 | 0x400B | 0x400F =>
      val i = address >> 2 & 3
      if ((lengthEnabled >> i & 1) != 0) length(i) = lengthTable(value >> 3)
    case 0x4014 => dmaPage = Some(value)
    case 0x4015 =>
      lengthEnabled = value & 0x0F
      for (i <- 0 until 4 if (value >> i & 1) == 0) length(i) = 0
    case 0x4016 => strobe = (value & 1) != 0; if (strobe) shift = buttons
    case 0x4017 =>
      fiveStep = (value & 0x80) != 0
      irqInhibit = (value & 0x40) != 0
      if (irqInhibit) frameIrq = false
      counterReset = if ((cycles & 1) == 0) 3 else 4
    case a if a < 0x6000 =>
    case a if a < 0x8000 => prgRam(a & 0x1FFF) = value
    case _ => if (cart.mapper == 3) ppu.chrBankSelect = value & 3
  }

  def step(): Unit = cpu.step()
  /** Runs until the PPU has finished `n` more frames. */
  def runFrames(n: Int): Unit = { val end = ppu.frames + n; while (ppu.frames < end) step() }
}
