package retro.gb

/** The DMG APU, as a reference model: a port of SameBoy's `apu.c` (DMG paths only), which is
  * the timing reference the Gambatte, blargg and SameSuite sound tests are checked against.
  *
  * The APU runs on 2 MHz ticks (`tick`, two per M-cycle); squares and sweep count at 1 MHz
  * through `lfDiv`. The frame sequencer ("DIV-APU") steps on falling edges of bit 12 of the timer
  * counter (`divEvent`) and arms envelopes on rising ones (`divSecondaryEvent`); `GbReference`
  * calls both, also when a DIV write resets the counter. Registers are FF10-FF3F (`read`,
  * `write`, offsets from FF10).
  *
  * `left` and `right` are the mixed output after each tick: per channel, a DAC that is on
  * outputs 15 - 2 * sample, scaled by NR50's volume where NR51 routes it; a DAC that is off
  * outputs nothing. */
class ApuReference(postBoot: Boolean) {
  import ApuReference._

  /** FF10-FF3F as last written (wave RAM at 0x20-0x2F). */
  val regs = Array.fill(0x30)(0)
  /** The CPU's address bus and PC, for the DMG's wave RAM read glitches. */
  var addressBus, pc = 0

  var enabled = false
  val active = Array.fill(4)(false)
  val samples = Array.fill(4)(0)
  var divDivider = 0
  var lfDiv = 1
  var skipDiv = SkipInactive

  // Channel 1's sweep
  var sweepCountdown = 0
  var sweepCalcCountdown = 0
  var sweepCalcReload = 0
  var sweepAddend = 0
  var shadowLength = 0
  var unshiftedSweep = false
  var sweepInstantDone = false
  var restartHold = 0
  var completedAddend = 0

  class Envelope {
    var locked, clock, shouldLock = false
    def set(value: Boolean, increase: Boolean, volume: Int): Unit = if (clock != value) {
      if (value) { clock = true; shouldLock = volume == 0xF && increase || volume == 0 && !increase }
      else { clock = false; locked |= shouldLock }
    }
    def clear(): Unit = { locked = false; clock = false; shouldLock = false }
  }

  class Square {
    var length = 0
    var volume = 0
    var volumeCountdown = 0
    var index = 0
    /** The duty bit, latched when the position steps: a duty write shows only at the next step. */
    var high = false
    var suppressed = false
    var countdown = 0
    var period = 0
    var lengthEnabled = false
    val envelope = new Envelope
    var didTick = false
    var justReloaded = false
    def clear(): Unit = {
      length = 0; volume = 0; volumeCountdown = 0; index = 0; high = false; suppressed = false; countdown = 0; period = 0
      lengthEnabled = false; envelope.clear(); didTick = false; justReloaded = false
    }
  }
  val square = Array.fill(2)(new Square)

  // Channel 3
  var waveEnable = false
  var waveLength = 0
  var waveShift = 4
  var wavePeriod = 0
  var waveLengthEnabled = false
  var waveCountdown = 0
  var waveIndex = 0
  var waveByte = 0
  var waveJustRead = false
  var wavePulsed = false
  var waveBuggedRead = 0

  // Channel 4
  var noiseLength = 0
  var noiseVolume = 0
  var noiseVolumeCountdown = 0
  var lfsr = 0
  var narrow = false
  var noiseCountdown = 0
  var noiseCounter = 0
  var noiseLengthEnabled = false
  var alignment = 0
  var lfsrSample = false
  var didStepCounter = false
  var countdownReloaded = false
  var delayedStart = 0
  val noiseEnvelope = new Envelope
  var noiseCounterActive = false
  var noiseBackgroundActive = false
  var startedWithDacOff = false

  var left, right = 0

  if (postBoot) {
    // The boot ROM leaves channel 1 on (its second note, faded out) and the mixer at full volume.
    enabled = true
    divDivider = 1 // the frame sequencer step, from Gambatte's ch2_init_env_counter_timing tests
    for ((r, v) <- Seq(0x00 -> 0x80, 0x01 -> 0x80, 0x02 -> 0xF3, 0x03 -> 0xC1, 0x04 -> 0x87, 0x14 -> 0x77, 0x15 -> 0xF3, 0x16 -> 0x80))
      regs(r) = v
    active(0) = true
    square(0).period = 0x7C1
    square(0).length = 0x40
    square(0).envelope.locked = true
    // Its duty phase, from Gambatte's ch1_init_pos tests: position 4 steps to 5 one or two ticks after
    // the 0x16-loop wait's first retrigger.
    square(0).index = 2
    square(0).countdown = 12
    square(1).countdown = 0xFFFF
    mix()
  }

  private def nr(reg: Int) = regs(reg - 0x10)
  def dacOn(i: Int) = i match {
    case 0 => (regs(0x02) & 0xF8) != 0
    case 1 => (regs(0x07) & 0xF8) != 0
    case 2 => waveEnable
    case 3 => (regs(0x11) & 0xF8) != 0
  }

  /** SameBoy's `update_sample`: a DAC that is off holds its last sample. */
  private def update(i: Int, value: Int): Unit = if (dacOn(i)) samples(i) = value

  private def mix(): Unit = {
    var l, r = 0
    for (i <- 0 until 4 if dacOn(i)) {
      val a = 15 - 2 * samples(i)
      if ((regs(0x15) & (0x10 << i)) != 0) l += a * (((regs(0x14) >> 4) & 7) + 1)
      if ((regs(0x15) & (1 << i)) != 0) r += a * ((regs(0x14) & 7) + 1)
    }
    left = l; right = r
  }

  private def updateSquare(i: Int): Unit = if (!square(i).suppressed) update(i, if (square(i).high) square(i).volume else 0)

  private def updateWave(): Unit =
    update(2, (if ((waveIndex & 1) != 0) waveByte & 0xF else waveByte >> 4) >> waveShift)

  private def nrx2Glitch(volume: Int, value: Int, old: Int, countdown: Int, env: Envelope): (Int, Int) = {
    var v = volume
    var c = countdown
    def glitch(value: Int, old: Int): Unit = {
      if (env.clock) c = value & 7
      var shouldTick = (value & 7) != 0 && (old & 7) == 0 && !env.locked
      val shouldInvert = ((value ^ old) & 8) != 0
      if ((value & 0xF) == 8 && (old & 0xF) == 8 && !env.locked) shouldTick = true
      if (shouldInvert) {
        if ((value & 8) != 0) {
          if ((old & 7) == 0 && !env.locked) v ^= 0xF else v = (0xE - v) & 0xF
          shouldTick = false
        } else v = (0x10 - v) & 0xF
      }
      if (shouldTick) v = (if ((value & 8) != 0) v + 1 else v - 1) & 0xF
      else if ((value & 7) == 0 && env.clock) env.set(false, false, 0)
    }
    // The DMG goes through FF on the way.
    glitch(0xFF, old)
    glitch(value, 0xFF)
    (v, c)
  }

  private def tickSquareEnvelope(i: Int): Unit = {
    val s = square(i)
    s.envelope.set(false, false, 0)
    if (s.envelope.locked) return
    val nrx2 = regs(if (i == 0) 0x02 else 0x07)
    if ((nrx2 & 7) == 0) return
    s.volume = (if ((nrx2 & 8) != 0) s.volume + 1 else s.volume - 1) & 0xF
    if (active(i)) updateSquare(i)
  }

  private def tickNoiseEnvelope(): Unit = {
    noiseEnvelope.set(false, false, 0)
    if (noiseEnvelope.locked) return
    val nr42 = regs(0x11)
    if ((nr42 & 7) == 0) return
    noiseVolume = (if ((nr42 & 8) != 0) noiseVolume + 1 else noiseVolume - 1) & 0xF
    if (active(3)) update(3, if ((lfsr & 1) != 0) noiseVolume else 0)
  }

  private def sweepCalculationDone(): Unit = {
    // APU bug: the frequency is checked after adding the sweep delta twice.
    if (restartHold == 0) shadowLength = square(0).period
    if ((regs(0x00) & 8) != 0) sweepAddend ^= 0x7FF
    if (shadowLength + sweepAddend > 0x7FF && (regs(0x00) & 8) == 0) { active(0) = false; update(0, 0) }
    completedAddend = sweepAddend
  }

  private def triggerSweepCalculation(duringDivWrite: Boolean): Unit = {
    val nr10 = regs(0x00)
    if ((nr10 & 0x70) != 0 && sweepCountdown == 7) {
      if ((nr10 & 7) != 0)
        square(0).period = (sweepAddend + shadowLength + (if ((nr10 & 8) != 0) 1 else 0)) & 0x7FF
      if (restartHold == 0) sweepAddend = square(0).period >> (nr10 & 7)
      sweepCalcCountdown = nr10 & 7
      sweepCalcReload = if (duringDivWrite) 1 else 1 + lfDiv
      unshiftedSweep = (nr10 & 7) == 0
      sweepCountdown = ((nr10 >> 4) & 7) ^ 7
      if (sweepCalcCountdown == 0) sweepInstantDone = true
    }
  }

  /** A falling edge of the counter's bit 12: the frame sequencer steps. */
  def divEvent(duringDivWrite: Boolean = false): Unit = {
    if (!enabled) return
    if (skipDiv == SkipSkip) { skipDiv = SkipSkipped; return }
    if (skipDiv == SkipSkipped) skipDiv = SkipInactive else divDivider = (divDivider + 1) & 0xFF

    if ((divDivider & 7) == 7) {
      for (s <- square if !s.envelope.clock) s.volumeCountdown = (s.volumeCountdown - 1) & 7
      if (!noiseEnvelope.clock) noiseVolumeCountdown = (noiseVolumeCountdown - 1) & 7
    }
    for (i <- 0 to 1 if square(i).envelope.clock) tickSquareEnvelope(i)
    if (noiseEnvelope.clock) tickNoiseEnvelope()

    if ((divDivider & 1) == 1) {
      for (i <- 0 to 1) {
        val s = square(i)
        if (s.lengthEnabled && s.length != 0) { s.length -= 1; if (s.length == 0) { active(i) = false; update(i, 0) } }
      }
      if (waveLengthEnabled && waveLength != 0) { waveLength -= 1; if (waveLength == 0) { active(2) = false; update(2, 0) } }
      if (noiseLengthEnabled && noiseLength != 0) { noiseLength -= 1; if (noiseLength == 0) { active(3) = false; update(3, 0) } }
    }

    if ((divDivider & 3) == 3) {
      sweepCountdown = (sweepCountdown + 1) & 7
      triggerSweepCalculation(duringDivWrite)
    }
    mix()
  }

  /** A rising edge of the counter's bit 12: envelopes whose period ran out are armed. */
  def divSecondaryEvent(): Unit = {
    if (!enabled) return
    for (i <- 0 to 1) {
      val s = square(i)
      val nrx2 = regs(if (i == 0) 0x02 else 0x07)
      if (active(i) && s.volumeCountdown == 0) {
        s.volumeCountdown = nrx2 & 7
        s.envelope.set(s.volumeCountdown != 0, (nrx2 & 8) != 0, s.volume)
      }
    }
    if (active(3) && noiseVolumeCountdown == 0) {
      noiseVolumeCountdown = regs(0x11) & 7
      noiseEnvelope.set(noiseVolumeCountdown != 0, (regs(0x11) & 8) != 0, noiseVolume)
    }
  }

  private def updateLfsr(): Unit = {
    lfsrSample = (lfsr & 1) != 0
    if (active(3)) update(3, if (lfsrSample) noiseVolume else 0)
  }

  private def stepLfsr(): Unit = {
    val highMask = if (narrow) 0x4040 else 0x4000
    val newHigh = ((lfsr ^ (lfsr >> 1) ^ 1) & 1) != 0
    lfsr >>= 1
    lfsr = if (newHigh) lfsr | highMask else lfsr & ~highMask
    updateLfsr()
  }

  /** One 2 MHz tick. */
  def tick(): Unit = {
    if (waveBuggedRead != 0) {
      waveBuggedRead -= 1
      if (waveBuggedRead == 0) {
        waveByte = regs(0x20 + (addressBus & 0xF))
        if (active(2)) updateWave()
      }
    }

    var startNoise = false
    if (delayedStart != 0) {
      delayedStart -= 1
      if (delayedStart == 0) startNoise = true
    }
    lfDiv ^= 1
    alignment = (alignment + 1) & 0xFF

    var sweepCycles = if (lfDiv == 0) 1 else 0
    if (sweepCalcReload > sweepCycles) { sweepCalcReload -= sweepCycles; sweepCycles = 0 }
    else {
      if (sweepCalcReload != 0 && sweepCalcCountdown == 0 && sweepInstantDone) sweepCalculationDone()
      sweepInstantDone = false
      sweepCycles -= sweepCalcReload
      sweepCalcReload = 0
    }
    if (sweepCalcCountdown != 0 && ((regs(0x00) & 7) != 0 || unshiftedSweep)) {
      if (sweepCalcCountdown > sweepCycles) sweepCalcCountdown -= sweepCycles
      else { sweepCalcCountdown = 0; sweepCalculationDone() }
    }
    if (restartHold != 0) restartHold -= 1

    for (i <- 0 to 1 if active(i)) {
      val s = square(i)
      if (s.countdown == 0) {
        s.countdown = (s.period ^ 0x7FF) * 2 + 1
        s.index = (s.index + 1) & 7
        s.high = duties(s.index + (regs(if (i == 0) 0x01 else 0x06) >> 6) * 8) != 0
        s.suppressed = false
        s.didTick = true
        updateSquare(i)
        s.justReloaded = true
      } else {
        s.justReloaded = false
        s.countdown -= 1
      }
    }

    waveJustRead = false
    if (active(2)) {
      if (waveCountdown == 0) {
        waveCountdown = waveLength11 ^ 0x7FF
        waveIndex = (waveIndex + 1) & 0x1F
        waveByte = regs(0x20 + (waveIndex >> 1))
        updateWave()
        waveJustRead = true
      } else waveCountdown -= 1
    } else if (waveEnable && wavePulsed) {
      if (waveCountdown == 0) {
        waveCountdown = waveLength11 ^ 0x7FF
        waveBuggedRead = 1
      } else waveCountdown -= 1
      if (waveCountdown == 0) waveBuggedRead = 2
    }

    if (noiseCounterActive || noiseBackgroundActive) {
      val divisor = if ((regs(0x12) & 7) == 0) 2 else (regs(0x12) & 7) << 2
      if (noiseCountdown == 0) noiseCountdown = divisor
      if (noiseCountdown == 1) {
        noiseCountdown = divisor
        val mask = 1 << (regs(0x12) >> 4)
        val oldBit = (noiseCounter & mask) != 0
        noiseCounter = (noiseCounter + 1) & 0x3FFF
        didStepCounter = true
        val newBit = (noiseCounter & mask) != 0
        if (newBit && !oldBit && active(3)) stepLfsr()
        countdownReloaded = true
      } else {
        noiseCountdown -= 1
        countdownReloaded = false
      }
    }
    if (startNoise) write(0x13, regs(0x13) | 0x80)
    mix()
  }

  /** NR33/NR34's 11-bit wave period. */
  private def waveLength11 = wavePeriod

  def read(reg: Int): Int = {
    if (reg == 0x16) {
      var v = 0x70 | (if (enabled) 0x80 else 0)
      for (i <- 0 until 4 if active(i)) v |= 1 << i
      return v
    }
    var r = reg
    if (r >= 0x20 && active(2)) {
      if (!waveJustRead) return 0xFF
      r = 0x20 + waveIndex / 2
    }
    regs(r) | (if (r < 0x20) readMask(r) else 0)
  }

  private def lengthGlitch(value: Int, lengthEnabled: Boolean, length: Int, reload: Int, stop: () => Unit): Int = {
    // APU glitch: enabling the length while the frame sequencer's next step clocks lengths ticks it once.
    if ((value & 0x40) != 0 && !lengthEnabled && (divDivider & 1) != 0 && length != 0) {
      val l = length - 1
      if (l == 0) { if ((value & 0x80) != 0) reload else { stop(); 0 } } else l
    } else length
  }

  private def prepareNoiseStart(): Unit = {
    noiseCounterActive = (regs(0x11) & 0xF8) != 0
    val wasStartedWithDacOff = startedWithDacOff
    startedWithDacOff = !noiseCounterActive
    var divisor = regs(0x12) & 7
    val wasBackground = noiseBackgroundActive
    noiseBackgroundActive = true
    var instantStep = false
    var div1Glitch = false

    if (divisor > 1 && noiseCountdown == 1) noiseCounter = (noiseCounter + 1) & 0x3FFF
    else if (noiseCountdown == 2 && (alignment & 3) == 0 && active(3)) {
      if (divisor == 0) divisor = 8
      else if (divisor == 1) {
        if (!didStepCounter) div1Glitch = true
        val mask = 1 << (regs(0x12) >> 4)
        val oldBit = (noiseCounter & mask) != 0
        noiseCounter = (noiseCounter + 1) & 0x3FFF
        if ((noiseCounter & mask) != 0 && !oldBit) instantStep = true
      }
    }
    noiseCountdown = if (divisor == 0) 6 else divisor * 4 + 6
    if ((alignment & 1) != 0) {
      if (divisor == 0) noiseCountdown += 1
      else if ((alignment & 2) != 0) { if (divisor == 1 && !active(3)) noiseCountdown += 1 else noiseCountdown -= 3 }
      else { noiseCountdown -= 1; if (divisor == 1 && active(3)) noiseCountdown -= 4 }
    } else if (divisor != 0) {
      if ((alignment & 2) != 0) noiseCountdown -= 2
      else if (divisor > 1) noiseCountdown -= 4
      else if (divisor == 1 && active(3) && (regs(0x12) & 0xF0) == 0) noiseCountdown -= 4
    }
    if (divisor > 1) { if (!noiseCounterActive && (alignment & 3) == 0) noiseCountdown += 4 }
    else if (wasBackground && !active(3) && (alignment & 3) == 0) {
      if (divisor == 0) { if (wasStartedWithDacOff) noiseCountdown += 28 }
      else noiseCountdown -= 4
    }
    if (div1Glitch) noiseCountdown -= 4
    lfsr = if (divisor == 0 && active(3) && (alignment & 3) == 3) 0x0055 else 0
    if (instantStep) stepLfsr()
  }

  /** SameBoy's `nr43_write` for pre-CGB-D models, without the CGB-D/E glitch tables. */
  private def nr43Write(value: Int): Unit = {
    narrow = (value & 8) != 0
    val old = regs(0x12)
    regs(0x12) = value
    if ((old & 0xF0) == (value & 0xF0)) return
    var effective = noiseCounter
    if (countdownReloaded) effective |= (effective - 1) & 0x3FFF
    val oldBit = ((effective >> (old >> 4)) & 1) != 0
    val glitchValue = (old & 0x7F) | (value & 0x80)
    val glitchBit = ((effective >> (glitchValue >> 4)) & 1) != 0
    val newBit = ((effective >> (value >> 4)) & 1) != 0
    if (oldBit == newBit && newBit != glitchBit) { if (!newBit) stepLfsr() }
    else if (!oldBit && newBit) {
      val previousNarrow = narrow
      narrow = true
      stepLfsr()
      narrow = previousNarrow
      if ((value & 0xF0) <= 0x20 && glitchBit && (effective & 8) == 0) {
        stepLfsr()
        lfsr &= ~(if (narrow) 0x4040 else 0x4000)
        lfsr |= (lfsr & (if (narrow) 0x2020 else 0x2000)) << 1
      }
    } else if ((value & 0xF0) <= 0x20 && !glitchBit && !newBit && !oldBit && (effective & 8) != 0) stepLfsr()
  }

  /** A CPU write to FF10 + `reg`. */
  def write(reg: Int, data: Int, duringDivWrite: Boolean = false): Unit = {
    var value = data
    var r = reg
    if (!enabled && r != 0x16 && r < 0x20 && r != 0x01 && r != 0x06 && r != 0x0B && r != 0x10) return
    if (r >= 0x20 && active(2)) {
      if (!waveJustRead) return
      r = 0x20 + waveIndex / 2
    }
    r match {
      case 0x14 | 0x15 =>
      case 0x16 =>
        val oldLengths = (square(0).length, square(1).length, waveLength, noiseLength)
        if ((value & 0x80) != 0 && !enabled) powerOn()
        else if ((value & 0x80) == 0 && enabled) powerOff()
        if ((value & 0x80) != 0) {
          square(0).length = oldLengths._1; square(1).length = oldLengths._2; waveLength = oldLengths._3; noiseLength = oldLengths._4
        }
      case 0x00 =>
        if (sweepCalcCountdown != 0 || sweepCalcReload != 0) nr10Glitch(value)
        regs(0x00) = value
        if (shadowLength + completedAddend + 1 > 0x7FF && (value & 8) == 0) { active(0) = false; update(0, 0) }
        triggerSweepCalculation(duringDivWrite)
      case 0x01 | 0x06 =>
        square(if (r == 0x06) 1 else 0).length = 0x40 - (value & 0x3F)
        if (!enabled) value &= 0x3F
      case 0x02 | 0x07 =>
        val i = if (r == 0x07) 1 else 0
        val s = square(i)
        if ((value & 0xF8) == 0) { regs(r) = value; active(i) = false; update(i, 0) }
        else if (active(i)) {
          val (v, c) = nrx2Glitch(s.volume, value, regs(r), s.volumeCountdown, s.envelope)
          s.volume = v; s.volumeCountdown = c
          updateSquare(i)
        }
      case 0x03 | 0x08 =>
        val s = square(if (r == 0x08) 1 else 0)
        s.period = (s.period & ~0xFF) | value
        if (s.justReloaded) s.countdown = (s.period ^ 0x7FF) * 2 + 1
      case 0x04 | 0x09 =>
        val i = if (r == 0x09) 1 else 0
        val s = square(i)
        val wasActive = active(i)
        if ((value & 0x80) == 0 && active(i) && (regs(r) & 7) == 7 && (value & 7) != 7 && (s.countdown & 1) != 0 &&
          s.didTick && (s.countdown >> 1) == (s.period ^ 0x7FF)) {
          s.index = (s.index - 1) & 7
          s.suppressed = false
        }
        s.period = (s.period & 0xFF) | (value & 7) << 8
        if (s.justReloaded) s.countdown = (s.period ^ 0x7FF) * 2 + 1
        if ((value & 0x80) != 0) {
          s.envelope.locked = false
          s.envelope.clock = false
          s.didTick = false
          // Timing quirk: if already active, the sound starts 2 ticks earlier.
          s.countdown = (s.period ^ 0x7FF) * 2 + (if (!active(i)) 6 - lfDiv else 4 - lfDiv)
          val nrx2 = regs(if (i == 0) 0x02 else 0x07)
          s.volume = nrx2 >> 4
          if (active(i)) updateSquare(i)
          s.volumeCountdown = nrx2 & 7
          if ((nrx2 & 0xF8) != 0 && !active(i)) { active(i) = true; update(i, 0); s.suppressed = true }
          if (s.length == 0) { s.length = 0x40; s.lengthEnabled = false }
          if (i == 0) {
            sweepInstantDone = false
            shadowLength = 0
            completedAddend = 0
            if ((regs(0x00) & 7) != 0) {
              // APU bug: with a nonzero shift, the overflow check also happens on trigger.
              sweepCalcCountdown = regs(0x00) & 7
              sweepCalcReload = (if (lfDiv == 0) 3 else 2) + (if (!wasActive) 1 else 0)
              unshiftedSweep = false
              sweepAddend = s.period >> (regs(0x00) & 7)
            } else sweepAddend = 0
            restartHold = 2 - lfDiv
            sweepCountdown = ((regs(0x00) >> 4) & 7) ^ 7
          }
        }
        s.length = lengthGlitch(value, s.lengthEnabled, s.length, 0x3F, () => { active(i) = false; update(i, 0) })
        s.lengthEnabled = (value & 0x40) != 0
      case 0x0A =>
        waveEnable = (value & 0x80) != 0
        if (!waveEnable) {
          wavePulsed = false
          if (active(2)) {
            if (waveCountdown == 0) waveByte = regs(0x20 + (pc & 0xF))
            else if (waveJustRead) waveByte = regs(0x20 + 0xA)
          }
          active(2) = false
          update(2, 0)
        }
      case 0x0B => waveLength = 0x100 - value
      case 0x0C =>
        waveShift = Array(4, 0, 1, 2)((value >> 5) & 3)
        if (active(2)) updateWave()
      case 0x0D => wavePeriod = (wavePeriod & ~0xFF) | value
        if (waveBuggedRead == 1) waveCountdown = wavePeriod ^ 0x7FF
      case 0x0E =>
        wavePeriod = (wavePeriod & 0xFF) | (value & 7) << 8
        if ((value & 0x80) != 0) {
          wavePulsed = true
          // DMG bug: retriggering 1 cycle before the APU reads wave RAM corrupts it.
          if (active(2) && waveCountdown == 0) {
            val offset = ((waveIndex + 1) >> 1) & 0xF
            if (offset < 4) regs(0x20) = regs(0x20 + offset)
            else for (k <- 0 until 4) regs(0x20 + k) = regs(0x20 + (offset & ~3) + k)
          }
          waveIndex = 0
          if (active(2) && waveCountdown == 0) waveByte = regs(0x20)
          if (waveEnable) { active(2) = true; update(2, (waveByte >> 4) >> waveShift) }
          waveCountdown = (wavePeriod ^ 0x7FF) + 3
          if (waveLength == 0) { waveLength = 0x100; waveLengthEnabled = false }
        }
        waveLength = lengthGlitch(value, waveLengthEnabled, waveLength, 0xFF, () => { active(2) = false; update(2, 0) })
        waveLengthEnabled = (value & 0x40) != 0
      case 0x10 => noiseLength = 0x40 - (value & 0x3F)
      case 0x11 =>
        if ((value & 0xF8) == 0) {
          if (active(3) && (regs(0x12) & 7) != 0) {
            if (noiseCountdown <= 2) noiseCounter = (noiseCounter + 1) & 0x3FFF
            noiseBackgroundActive = false
          }
          regs(r) = value
          active(3) = false
          update(3, 0)
          noiseCounterActive = false
        } else if (active(3)) {
          val (v, c) = nrx2Glitch(noiseVolume, value, regs(r), noiseVolumeCountdown, noiseEnvelope)
          noiseVolume = v; noiseVolumeCountdown = c
          update(3, if (lfsrSample) noiseVolume else 0)
        }
      case 0x12 =>
        if (countdownReloaded) {
          val divisor = if ((value & 7) == 0) 2 else (value & 7) << 2
          noiseCountdown = divisor + (if (divisor == 2) 0 else Array(2, 1, 4, 3)(alignment & 3))
          val oldBit = ((noiseCounter >> (regs(0x12) >> 4)) & 1) != 0
          val glitchBit = ((noiseCounter >> 7) & 1) != 0
          val newBit = ((noiseCounter >> (value >> 4)) & 1) != 0
          if (!oldBit && newBit && glitchBit) {
            val previous = (noiseCounter - 1) & 0x3FFF
            if (((previous >> (regs(0x12) >> 4)) & 1) != 0 && ((previous >> (value >> 4)) & 1) == 0 && ((previous >> 7) & 1) != 0)
              stepLfsr()
          }
        }
        nr43Write(0xFF)
        nr43Write(value)
      case 0x13 =>
        if ((value & 0x80) != 0) {
          noiseEnvelope.locked = false
          noiseEnvelope.clock = false
          if ((alignment & 3) != 0) delayedStart = 6
          else {
            lfsr = 0
            prepareNoiseStart()
            noiseVolume = regs(0x11) >> 4
            lfsrSample = false
            noiseVolumeCountdown = regs(0x11) & 7
            didStepCounter = (alignment & 3) == 2
            if ((regs(0x11) & 0xF8) != 0) { active(3) = true; update(3, 0) }
            if (noiseLength == 0) { noiseLength = 0x40; noiseLengthEnabled = false }
          }
        }
        noiseLength = lengthGlitch(value, noiseLengthEnabled, noiseLength, 0x3F, () => { active(3) = false; update(3, 0) })
        noiseLengthEnabled = (value & 0x40) != 0
      case _ =>
    }
    regs(r) = value
    mix()
  }

  private def nr10Glitch(value: Int): Unit = {
    if (sweepCalcReload == 1 && lfDiv == 0) {}
    else if (sweepCalcReload > 1) {}
    else if (sweepCalcCountdown != 0 && (regs(0x00) & 7) == 0 && lfDiv != 0) {
      sweepCalcCountdown -= 1
      if (sweepCalcCountdown <= 1) { sweepCalcCountdown = 0; sweepCalculationDone() }
    }
  }

  /** The counter whose bit 12 clocks the frame sequencer, when NR52 turns the APU on. */
  var counter = 0

  private def powerOn(): Unit = {
    clearState()
    lfDiv = 1
    waveShift = 4
    // APU glitch: turning the APU on while bit 12 is set skips the first frame sequencer step.
    // It sees the counter one M-cycle later (Gambatte's ch2_init_reset_env_counter_timing_5).
    if (((counter + 4) & 0x1000) != 0) { skipDiv = SkipSkip; divDivider = 1 }
    square(0).countdown = 0xFFFF
    square(1).countdown = 0xFFFF
    enabled = true
  }

  private def powerOff(): Unit = {
    for (i <- 0 until 4) update(i, 0)
    clearState()
    for (k <- 0 until 0x20) regs(k) = 0
    enabled = false
  }

  private def clearState(): Unit = {
    enabled = false
    for (i <- 0 until 4) { active(i) = false; samples(i) = 0 }
    divDivider = 0; lfDiv = 0; skipDiv = SkipInactive
    sweepCountdown = 0; sweepCalcCountdown = 0; sweepCalcReload = 0; sweepAddend = 0; shadowLength = 0
    unshiftedSweep = false; sweepInstantDone = false; restartHold = 0; completedAddend = 0
    square.foreach(_.clear())
    waveEnable = false; waveLength = 0; waveShift = 0; wavePeriod = 0; waveLengthEnabled = false; waveCountdown = 0
    waveIndex = 0; waveByte = 0; waveJustRead = false; wavePulsed = false; waveBuggedRead = 0
    noiseLength = 0; noiseVolume = 0; noiseVolumeCountdown = 0; lfsr = 0; narrow = false; noiseCountdown = 0
    noiseCounter = 0; noiseLengthEnabled = false; alignment = 0; lfsrSample = false; didStepCounter = false
    countdownReloaded = false; delayedStart = 0; noiseEnvelope.clear(); noiseCounterActive = false
    noiseBackgroundActive = false; startedWithDacOff = false
  }
}

object ApuReference {
  val SkipInactive = 0
  val SkipSkipped = 1
  val SkipSkip = 2
  val duties = Array(
    0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 1, 1, 1,
    0, 1, 1, 1, 1, 1, 1, 0)
  /** Bits that read as 1, FF10-FF2F. */
  val readMask = Array(
    0x80, 0x3F, 0x00, 0xFF, 0xBF,
    0xFF, 0x3F, 0x00, 0xFF, 0xBF,
    0x7F, 0xFF, 0x9F, 0xFF, 0xBF,
    0xFF, 0xFF, 0x00, 0x00, 0xBF,
    0x00, 0x00, 0x70, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)
}
