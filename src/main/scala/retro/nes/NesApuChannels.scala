package retro.nes

/** The APU's tone channels as `NesReference` models them (their length counters live there,
  * with the frame counter). Each channel's `cycle()` runs once per CPU cycle, `quarter()` and
  * `half()` on the frame counter's clocks, `write(reg, value)` on a CPU write to one of its four
  * registers. Behavior follows the nesdev wiki's APU pages. */
final class NesEnvelope {
  var start, loop, constant = false
  var volume, divider, decay = 0
  def write(value: Int): Unit = { loop = (value & 0x20) != 0; constant = (value & 0x10) != 0; volume = value & 0x0F }
  def quarter(): Unit =
    if (start) { start = false; decay = 15; divider = volume }
    else if (divider == 0) { divider = volume; if (decay > 0) decay -= 1 else if (loop) decay = 15 }
    else divider -= 1
  def output = if (constant) volume else decay
}

/** Pulse 1 (`first`) or 2: an 8-step duty sequencer clocked every 2 (period + 1) CPU cycles,
  * the envelope, and the sweep unit, whose negate is ones' complement on pulse 1. */
final class NesPulse(first: Boolean) {
  val envelope = new NesEnvelope
  var duty, position, period, timer = 0
  var sweepEnabled, sweepNegate, sweepReload = false
  var sweepPeriod, sweepShift, sweepDivider = 0
  def target = {
    val change = period >> sweepShift
    if (sweepNegate) period - change - (if (first) 1 else 0) else period + change
  }
  def muted = period < 8 || target > 0x7FF
  def write(reg: Int, value: Int): Unit = reg match {
    case 0 => duty = value >> 6; envelope.write(value)
    case 1 =>
      sweepEnabled = (value & 0x80) != 0; sweepPeriod = value >> 4 & 7
      sweepNegate = (value & 0x08) != 0; sweepShift = value & 7; sweepReload = true
    case 2 => period = period & 0x700 | value
    case _ => period = period & 0xFF | (value & 7) << 8; position = 0; envelope.start = true
  }
  def cycle(): Unit =
    if (timer == 0) { timer = 2 * period + 1; position = (position - 1) & 7 }
    else timer -= 1
  def quarter(): Unit = envelope.quarter()
  def half(): Unit = {
    if (sweepDivider == 0 && sweepEnabled && sweepShift != 0 && !muted) period = target
    if (sweepDivider == 0 || sweepReload) { sweepDivider = sweepPeriod; sweepReload = false }
    else sweepDivider -= 1
  }
  /** 0-15; `playing` is the length counter's being nonzero. */
  def output(playing: Boolean) = if (!playing || muted || (NesPulse.duties(duty) >> position & 1) == 0) 0 else envelope.output
}
object NesPulse {
  /** The duty sequences, bit n for sequencer step n (the sequencer counts down from 0). */
  val duties = Array(0x80, 0xC0, 0xF0, 0x3F)
}

/** The triangle: a 32-step sequencer clocked every period + 1 CPU cycles while both the length
  * and linear counters are nonzero; it holds its last step when silenced. */
final class NesTriangle {
  var control, reloadLinear = false
  var linearLoad, linear, period, timer, position = 0
  def write(reg: Int, value: Int): Unit = reg match {
    case 0 => control = (value & 0x80) != 0; linearLoad = value & 0x7F
    case 2 => period = period & 0x700 | value
    case 3 => period = period & 0xFF | (value & 7) << 8; reloadLinear = true
    case _ =>
  }
  def cycle(playing: Boolean): Unit =
    if (timer == 0) { timer = period; if (playing && linear > 0) position = (position + 1) & 31 }
    else timer -= 1
  def quarter(): Unit = {
    if (reloadLinear) linear = linearLoad else if (linear > 0) linear -= 1
    if (!control) reloadLinear = false
  }
  def output = if (position < 16) 15 - position else position - 16
}

/** The noise channel: a 15-bit LFSR (feedback from bit 1, or bit 6 in mode 1) clocked from a
  * period table in CPU cycles (`NesTiming.noisePeriods`), and the envelope. */
final class NesNoise(periods: Seq[Int] = NesNoise.periods.toSeq) {
  val envelope = new NesEnvelope
  var mode = false
  var period = periods(0) - 1
  var timer = 0
  var lfsr = 1
  def write(reg: Int, value: Int): Unit = reg match {
    case 0 => envelope.write(value)
    case 2 => mode = (value & 0x80) != 0; period = periods(value & 0x0F) - 1
    case 3 => envelope.start = true
    case _ =>
  }
  def cycle(): Unit = if (timer == 0) {
    timer = period
    val feedback = (lfsr ^ (lfsr >> (if (mode) 6 else 1))) & 1
    lfsr = lfsr >> 1 | feedback << 14
  } else timer -= 1
  def quarter(): Unit = envelope.quarter()
  def output(playing: Boolean) = if (!playing || (lfsr & 1) != 0) 0 else envelope.output
}
object NesNoise {
  val periods = NesTiming.ntsc.noisePeriods.toArray
}

/** The APU's non-linear mixer, from the nesdev wiki's formulas, scaled so the loudest output is
  * just under 65536. Pulse depends only on p1 + p2 (31 entries). Triangle, noise and DMC go
  * through one table indexed by t / 8227 + n / 12241 + d / 22638 in units of 1 / (32 * 22638),
  * that is 88 t + 59 n + 32 d (weights within 0.3%): the common 3 t + 2 n + d table is off by 9%
  * in the triangle's weight, which blargg's apu_mixer/triangle hears. */
object NesMixer {
  val pulse = Array.tabulate(31)(n => if (n == 0) 0 else math.round(65535 * 95.88 / (8128.0 / n + 100)).toInt)
  val tndWeights = (88, 59, 32)
  val tnd = Array.tabulate(88 * 15 + 59 * 15 + 32 * 127 + 1)(l => if (l == 0) 0 else math.round(65535 * 159.79 / (32 * 22638.0 / l + 100)).toInt)
  def mix(pulse1: Int, pulse2: Int, triangle: Int, noise: Int, dmc: Int) =
    pulse(pulse1 + pulse2) + tnd(88 * triangle + 59 * noise + 32 * dmc)
}
