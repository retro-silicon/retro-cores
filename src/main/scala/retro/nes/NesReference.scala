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
  * DMA follows the 2A03's DMA unit (as Mesen models it): OAM DMA (a write to 4014) and the
  * DMC's sample fetches halt the CPU on its next read. The halted read is repeated in the halt,
  * dummy and alignment cycles, side effects included (4016/4017 only on the halt cycle). Reads
  * happen on get cycles (odd cycle numbers), OAM writes on put cycles; when both run, the DMC
  * read takes a get cycle from the OAM copy, its halt and dummy cycles overlapping the copy. */
class NesReference(val cart: NesCartridge, val dotsBefore: Int = 2, val timing: NesTiming = NesTiming.ntsc) {
  val ram = Array.fill(0x800)(0)
  val prgRam = Array.fill(0x2000)(0)
  val ppu = new NesPpuReference(cart, timing)
  var openBus = 0
  /** CPU cycles since power-on. */
  var cycles = 0L
  /** Controller 1 buttons (A B Select Start Up Down Left Right in bits 0-7). */
  var buttons = 0
  private var strobe = false
  private var shift = 0

  // ---- APU: frame counter, its IRQ, the four length counters and the tone channels -----------
  val pulse1 = new NesPulse(first = true)
  val pulse2 = new NesPulse(first = false)
  val triangle = new NesTriangle
  val noise = new NesNoise(timing.noisePeriods)
  /** The mixed output (see `NesMixer`) as the previous cycle left the APU. */
  var sample = 0
  /** Called with `sample` once per CPU cycle. */
  var onSample: Int => Unit = null
  /** Length counters of pulse 1, pulse 2, triangle and noise. */
  val length = Array.fill(4)(0)
  private val lengthHalt = Array.fill(4)(false)
  /** Halt flags and length loads written by the CPU apply after the next cycle's APU clock: a
    * length clock in that cycle still sees the old halt flag, and a load is dropped when that
    * clock decremented the counter (blargg's len_halt_timing and len_reload_timing). */
  private val newHalt = Array.fill(4)(false)
  private val lengthLoad = Array.fill(4)(-1)
  private val lengthBefore = Array.fill(4)(0)
  private var lengthEnabled = 0
  private val lengthTable = Array(10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
                                  12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30)
  private def quarterFrame(): Unit = { pulse1.quarter(); pulse2.quarter(); triangle.quarter(); noise.quarter() }
  private def halfFrame(): Unit = {
    for (i <- 0 until 4) if (length(i) > 0 && !lengthHalt(i)) length(i) -= 1
    pulse1.half(); pulse2.half()
  }

  var frameIrq = false
  private var frameCounter = 0
  private var fiveStep, irqInhibit = false
  /** Power-up acts as a 4017 write of 00 shortly before the reset sequence: the counter restarts
    * in the second cycle, which both blargg's apu_reset/4017_timing (9-12 cycles before the
    * first instruction) and blargg_apu_2005.07.30/09.reset_timing accept. */
  private var counterReset = NesReference.powerUpCounterReset
  /** One APU clock, ahead of the CPU access in the same cycle (so a 4015 read sees the flag
    * set in its own cycle). A 4017 write resets the counter 3 or 4 cycles later, by the parity
    * of the write cycle; the IRQ flag is then raised 29828-29830 cycles after the reset. */
  private def apuCycle(): Unit = {
    sample = NesMixer.mix(pulse1.output(length(0) > 0), pulse2.output(length(1) > 0), triangle.output, noise.output(length(3) > 0), dmcLevel)
    if (onSample != null) onSample(sample)
    dmcCycle()
    frameCounter += 1
    if (counterReset > 0) {
      counterReset -= 1
      if (counterReset == 0) { frameCounter = 0; if (fiveStep) { quarterFrame(); halfFrame() } }
    }
    val Seq(step1, step2, step3, step4) = timing.steps
    val step5 = timing.fifthStep
    if (frameCounter == step1 || frameCounter == step2 || frameCounter == step3 || frameCounter == (if (fiveStep) step5 else step4)) quarterFrame()
    if (!fiveStep) {
      if (frameCounter == step2 || frameCounter == step4) halfFrame()
      if (frameCounter >= step4 - 1 && frameCounter <= step4 + 1 && !irqInhibit) frameIrq = true
      if (frameCounter == step4 + 1) frameCounter = 0
    } else {
      if (frameCounter == step2 || frameCounter == step5) halfFrame()
      if (frameCounter == step5 + 1) frameCounter = 0
    }
    pulse1.cycle(); pulse2.cycle(); triangle.cycle(length(2) > 0); noise.cycle()
    for (i <- 0 until 4) {
      if (lengthLoad(i) >= 0 && length(i) == lengthBefore(i)) length(i) = lengthLoad(i)
      lengthLoad(i) = -1
      lengthHalt(i) = newHalt(i)
    }
  }

  // ---- DMC: its timer, sample buffer, memory reader and IRQ (no sound output yet) -----------
  private val dmcRates = timing.dmcRates.toArray
  var dmcIrq = false
  private var dmcIrqEnable, dmcLoop = false
  private var dmcPeriod = dmcRates(0) - 1
  private var dmcTimer = dmcPeriod
  private var dmcSampleAddress, dmcSampleLength, dmcAddress, dmcBytes = 0
  private var dmcBuffer, dmcShift = 0
  private var dmcBufferEmpty, dmcSilence = true
  private var dmcBits = 8
  /** The output level (0-127), for when sound is added. */
  var dmcLevel = 0
  private var dmcStartDelay, dmcDisableDelay = 0
  private def dmcRestart(): Unit = { dmcAddress = dmcSampleAddress; dmcBytes = dmcSampleLength }
  private def dmcCycle(): Unit = {
    if (dmcDisableDelay > 0) { dmcDisableDelay -= 1; if (dmcDisableDelay == 0) { dmcBytes = 0; stopDmcDma() } }
    if (dmcStartDelay > 0) { dmcStartDelay -= 1; if (dmcStartDelay == 0) startDmcDma() }
    if (dmcTimer > 0) dmcTimer -= 1
    else {
      dmcTimer = dmcPeriod
      if (!dmcSilence) {
        if ((dmcShift & 1) != 0) { if (dmcLevel <= 125) dmcLevel += 2 } else if (dmcLevel >= 2) dmcLevel -= 2
      }
      dmcShift >>= 1
      dmcBits -= 1
      if (dmcBits == 0) {
        dmcBits = 8
        if (dmcBufferEmpty) dmcSilence = true
        else { dmcSilence = false; dmcShift = dmcBuffer; dmcBufferEmpty = true; startDmcDma() }
      }
    }
  }
  private def dmcFetched(v: Int): Unit = if (dmcBytes > 0) {
    dmcBuffer = v
    dmcBufferEmpty = false
    dmcAddress = if (dmcAddress == 0xFFFF) 0x8000 else dmcAddress + 1
    dmcBytes -= 1
    if (dmcBytes == 0) { if (dmcLoop) dmcRestart() else if (dmcIrqEnable) dmcIrq = true }
  }

  // ---- DMA unit ------------------------------------------------------------------------------
  private var needHalt, needDummy, dmcDma, dmcAbort, oamDma = false
  private var oamPage = 0
  private def startDmcDma(): Unit = if (dmcBufferEmpty && dmcBytes > 0) { dmcDma = true; needDummy = true; needHalt = true }
  /** The DMC was disabled: a fetch that has not halted the CPU yet is dropped, one under way
    * gives up after its current cycle. */
  private def stopDmcDma(): Unit = if (dmcDma) {
    if (needHalt) { dmcDma = false; needDummy = false; needHalt = false } else dmcAbort = true
  }

  private def dma(cpuAddress: Int): Unit = {
    def stolen(access: => Int): Int = { val v = cycle(access); cpu.haltedCycle(); v }
    val inputPort = cpuAddress == 0x4016 || cpuAddress == 0x4017
    def dummy() = stolen(if (inputPort) openBus else load(cpuAddress))
    needHalt = false
    if (dmcAbort && dmcDma && !oamDma) { dmcDma = false; dmcAbort = false; stolen(openBus); return }
    stolen(load(cpuAddress))
    dmcAbort = false
    var count, index, value = 0
    def step(): Unit =
      if (dmcAbort) { dmcDma = false; dmcAbort = false; needDummy = false; needHalt = false }
      else if (needHalt) needHalt = false
      else if (needDummy) needDummy = false
    while (dmcDma || oamDma) {
      val get = (cycles & 1) == 1
      if (get && dmcDma && !needHalt && !needDummy) {
        step()
        val v = stolen(load(dmcAddress))
        dmcDma = false
        dmcAbort = false
        dmcFetched(v)
      } else if (get && oamDma) {
        step()
        value = stolen(load(oamPage << 8 | index))
        index += 1
        count += 1
      } else if (!get && oamDma && (count & 1) == 1) {
        step()
        stolen({ ppu.cpuWrite(4, value); value })
        count += 1
        if (count == 512) oamDma = false
      } else { step(); dummy() }
    }
  }

  val bus: R2A03Bus = new R2A03Bus {
    def read(address: Int) = {
      if (needHalt) dma(address)
      cycle(load(address))
    }
    def write(address: Int, value: Int) = cycle({ store(address, value); value })
  }
  val cpu = new R2A03Reference(bus, _ => (ppu.nmi, frameIrq || dmcIrq || cart.irq))

  /** One CPU cycle around `access`. */
  private def cycle(access: => Int): Int = {
    for (_ <- 0 until dotsBefore) ppu.tick()
    apuCycle()
    cart.cpuCycle()
    val v = access
    openBus = v
    for (_ <- dotsBefore until timing.dotsPerCycle((cycles % timing.dotsPerCycle.size).toInt)) ppu.tick()
    cycles += 1
    v
  }
  def load(address: Int): Int = address match {
    case a if a < 0x2000 => ram(a & 0x7FF)
    case a if a < 0x4000 => ppu.cpuRead(a)
    case 0x4015 =>
      val r = (if (dmcIrq) 0x80 else 0) | (if (frameIrq) 0x40 else 0) | (openBus & 0x20) | (if (dmcBytes > 0) 0x10 else 0) |
        (0 until 4).map(i => if (length(i) > 0) 1 << i else 0).sum
      frameIrq = false
      r
    case 0x4016 =>
      val bit = if (strobe) buttons & 1 else { val b = shift & 1; shift = (shift >> 1) | 0x80; b }
      (openBus & 0xE0) | bit
    case a if a < 0x6000 => openBus
    case a if a < 0x8000 => cart.lowRead(a, prgRam, openBus)
    case a => cart.prg(cart.prgIndex(a))
  }
  def store(address: Int, value: Int): Unit = {
    if (address >= 0x4000 && address < 0x4010) Seq(pulse1.write _, pulse2.write _, triangle.write _, noise.write _)(address >> 2 & 3)(address & 3, value)
    storeRegister(address, value)
  }
  private def storeRegister(address: Int, value: Int): Unit = address match {
    case a if a < 0x2000 => ram(a & 0x7FF) = value
    case a if a < 0x4000 => ppu.cpuWrite(a, value)
    case 0x4000 | 0x4004 | 0x400C => newHalt(address >> 2 & 3) = (value & 0x20) != 0
    case 0x4008 => newHalt(2) = (value & 0x80) != 0
    case 0x4003 | 0x4007 | 0x400B | 0x400F =>
      val i = address >> 2 & 3
      if ((lengthEnabled >> i & 1) != 0) { lengthLoad(i) = lengthTable(value >> 3); lengthBefore(i) = length(i) }
    case 0x4010 =>
      dmcIrqEnable = (value & 0x80) != 0
      dmcLoop = (value & 0x40) != 0
      dmcPeriod = dmcRates(value & 0x0F) - 1
      if (!dmcIrqEnable) dmcIrq = false
    case 0x4011 => dmcLevel = value & 0x7F
    case 0x4012 => dmcSampleAddress = 0xC000 | value << 6
    case 0x4013 => dmcSampleLength = value << 4 | 1
    case 0x4014 => oamDma = true; oamPage = value; needHalt = true
    case 0x4015 =>
      dmcIrq = false
      if ((value & 0x10) == 0) { if (dmcDisableDelay == 0) dmcDisableDelay = if ((cycles & 1) == 0) 2 else 3 }
      else if (dmcBytes == 0) { dmcRestart(); dmcStartDelay = if ((cycles & 1) == 0) 2 else 3 }
      lengthEnabled = value & 0x0F
      for (i <- 0 until 4 if (value >> i & 1) == 0) length(i) = 0
    case 0x4016 => strobe = (value & 1) != 0; if (strobe) shift = buttons
    case 0x4017 =>
      fiveStep = (value & 0x80) != 0
      irqInhibit = (value & 0x40) != 0
      if (irqInhibit) frameIrq = false
      counterReset = if ((cycles & 1) == 0) 3 else 4
    case a if a < 0x6000 =>
    case a if a < 0x8000 => cart.lowWrite(a, value, prgRam)
    case a => cart.cpuWrite(a, value, cycles)
  }

  def step(): Unit = cpu.step()
  /** The reset button, between two instructions: the fetched opcode is dropped for the reset
    * sequence, and the APU acts as if 4015 were written with 00 (length counters cleared and
    * disabled, the DMC stopped after a fixed 2-cycle delay, its IRQ cleared) and 4017 rewritten
    * with its mode (IRQ inhibit cleared) with the power-up delay; the frame IRQ flag is cleared.
    * The PPU, the triangle and the DMC's output level are left alone. `Nes` does the same when
    * its `reset_button` pin is pulsed, at its next opcode fetch. */
  def reset(): Unit = {
    cpu.reset()
    dmcIrq = false
    if (dmcDisableDelay == 0) dmcDisableDelay = 2
    lengthEnabled = 0
    for (i <- 0 until 4) length(i) = 0
    irqInhibit = false
    frameIrq = false
    counterReset = NesReference.powerUpCounterReset
  }
  /** Runs until the PPU has finished `n` more frames. */
  def runFrames(n: Int): Unit = { val end = ppu.frames + n; while (ppu.frames < end) step() }
}

object NesReference {
  val powerUpCounterReset = 2
}
