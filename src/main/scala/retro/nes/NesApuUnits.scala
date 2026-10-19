package retro.nes

import spinal.core._

/** The APU's tone channels in hardware, mirroring `NesPulse`, `NesTriangle` and `NesNoise`
  * (see `NesApuChannels.scala`). `NesApuPlugin` calls each unit's `tick` once per cycle with
  * the frame counter's clocks, then `write` for a CPU write to its registers; a write's
  * assignments come later and win, as the model's register store follows its APU clock. */
class EnvelopeUnit extends Area {
  val start, loop, constant = RegInit(False)
  val volume, divider, decay = Reg(UInt(4 bits)).init(0)
  def tick(quarter: Bool): Unit = when(quarter) {
    when(start) { start := False; decay := 15; divider := volume }
      .elsewhen(divider === 0) { divider := volume; when(decay =/= 0) { decay := decay - 1 } elsewhen (loop) { decay := 15 } }
      .otherwise { divider := divider - 1 }
  }
  def write(v: Bits): Unit = { loop := v(5); constant := v(4); volume := v(3 downto 0).asUInt }
  def output: UInt = constant ? volume | decay
}

class PulseUnit(first: Boolean) extends Area {
  val envelope = new EnvelopeUnit
  val duty = Reg(UInt(2 bits)).init(0)
  val position = Reg(UInt(3 bits)).init(0)
  val period = Reg(UInt(11 bits)).init(0)
  val timer = Reg(UInt(12 bits)).init(0)
  val sweepEnabled, sweepNegate, sweepReload = RegInit(False)
  val sweepPeriod, sweepShift, sweepDivider = Reg(UInt(3 bits)).init(0)

  val change = (period |>> sweepShift).resize(12)
  val target = sweepNegate ? (period.resize(12) - change - (if (first) 1 else 0)) | (period.resize(12) + change)
  val muted = period < 8 || target > 0x7FF
  /** The period after this cycle's sweep, which the timer reloads from and writes modify. */
  val periodSwept = UInt(11 bits)

  def tick(quarter: Bool, half: Bool): Unit = {
    envelope.tick(quarter)
    periodSwept := period
    when(half && sweepDivider === 0 && sweepEnabled && sweepShift =/= 0 && !muted) { periodSwept := target(10 downto 0) }
    period := periodSwept
    when(half) {
      when(sweepDivider === 0 || sweepReload) { sweepDivider := sweepPeriod; sweepReload := False }
        .otherwise { sweepDivider := sweepDivider - 1 }
    }
    when(timer === 0) { timer := (periodSwept @@ U"1"); position := position - 1 } otherwise { timer := timer - 1 }
  }
  def write(reg: Int, v: Bits): Unit = reg match {
    case 0 => duty := v(7 downto 6).asUInt; envelope.write(v)
    case 1 =>
      sweepEnabled := v(7); sweepPeriod := v(6 downto 4).asUInt; sweepNegate := v(3); sweepShift := v(2 downto 0).asUInt
      sweepReload := True
    case 2 => period := periodSwept(10 downto 8) @@ v.asUInt
    case _ => period := v(2 downto 0).asUInt @@ periodSwept(7 downto 0); position := 0; envelope.start := True
  }
  def output(playing: Bool): UInt = {
    val bit = Vec(NesPulse.duties.map(d => B(d, 8 bits)))(duty)(position)
    (playing && !muted && bit) ? envelope.output | U(0, 4 bits)
  }
}

class TriangleUnit extends Area {
  val control, reloadLinear = RegInit(False)
  val linearLoad, linear = Reg(UInt(7 bits)).init(0)
  val period, timer = Reg(UInt(11 bits)).init(0)
  val position = Reg(UInt(5 bits)).init(0)
  /** `playing`: the length counter after this cycle's clock is nonzero. */
  def tick(quarter: Bool, playing: Bool): Unit = {
    val linearTicked = UInt(7 bits)
    linearTicked := linear
    when(quarter) {
      when(reloadLinear) { linearTicked := linearLoad } elsewhen (linear =/= 0) { linearTicked := linear - 1 }
      when(!control) { reloadLinear := False }
    }
    linear := linearTicked
    when(timer === 0) {
      timer := period
      when(playing && linearTicked =/= 0) { position := position + 1 }
    } otherwise { timer := timer - 1 }
  }
  def write(reg: Int, v: Bits): Unit = reg match {
    case 0 => control := v(7); linearLoad := v(6 downto 0).asUInt
    case 2 => period := period(10 downto 8) @@ v.asUInt
    case 3 => period := v(2 downto 0).asUInt @@ period(7 downto 0); reloadLinear := True
    case _ =>
  }
  // A named 4-bit signal: inlined into the mixer's wider arithmetic, Verilog would widen the
  // operand before the inversion.
  val output = CombInit(position(4) ? position(3 downto 0) | ~position(3 downto 0))
}

class NoiseUnit extends Area {
  val envelope = new EnvelopeUnit
  val mode = RegInit(False)
  val period = Reg(UInt(12 bits)).init(NesNoise.periods(0) - 1)
  val timer = Reg(UInt(12 bits)).init(0)
  val lfsr = Reg(UInt(15 bits)).init(1)
  def tick(quarter: Bool): Unit = {
    envelope.tick(quarter)
    when(timer === 0) {
      timer := period
      lfsr := ((lfsr(0) ^ (mode ? lfsr(6) | lfsr(1))).asUInt @@ lfsr(14 downto 1))
    } otherwise { timer := timer - 1 }
  }
  def write(reg: Int, v: Bits): Unit = reg match {
    case 0 => envelope.write(v)
    case 2 => mode := v(7); period := Vec(NesNoise.periods.map(p => U(p - 1, 12 bits)))(v(3 downto 0).asUInt)
    case 3 => envelope.start := True
    case _ =>
  }
  def output(playing: Bool): UInt = (playing && !lfsr(0)) ? envelope.output | U(0, 4 bits)
}

/** `NesMixer` as two ROMs. */
class MixerUnit extends Area {
  require(NesMixer.pulse.last + NesMixer.tnd.last < 65536)
  val pulseRom = Mem(UInt(16 bits), NesMixer.pulse.map(v => U(v, 16 bits)).toSeq)
  val tndRom = Mem(UInt(16 bits), NesMixer.tnd.map(v => U(v, 16 bits)).toSeq)
  def mix(pulse1: UInt, pulse2: UInt, triangle: UInt, noise: UInt, dmc: UInt): UInt = {
    val (wt, wn, wd) = NesMixer.tndWeights
    val l = (triangle * U(wt, 7 bits)).resize(13) + (noise * U(wn, 6 bits)).resize(13) + (dmc * U(wd, 6 bits)).resize(13)
    pulseRom.readAsync(pulse1 +^ pulse2) + tndRom.readAsync(l)
  }
}
