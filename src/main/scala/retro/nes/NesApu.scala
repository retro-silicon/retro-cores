package retro.nes

import retro.core._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

/** The 2A03's APU and DMA unit, mirroring `NesReference`: the frame counter with its IRQ, the
  * length counters (4015 reads them), the two pulses, the triangle and the noise channel
  * (the units below), the DMC (its timer, sample buffer, memory reader, output level and
  * IRQ), the mixer (`audio`, one sample per cycle from the state at the start of the cycle),
  * and OAM DMA (4014). Reads of the other registers in 4000-4017 (4016 is `NesJoypadPlugin`'s)
  * are open bus.
  *
  * Each cycle the APU ticks first and the access sees the ticked state. Length halt flags and
  * loads take effect after the next cycle's tick. A 4017 write resets the
  * frame counter 3 cycles later when it lands on an even cycle, 4 on an odd one (counting
  * cycles from the first reset cycle), and with bit 7 set clocks the length counters then.
  *
  * The DMA unit is a bus master above the CPU, mirroring `NesReference`: OAM DMA and DMC sample
  * fetches halt the CPU on its next read; the halted read is repeated in the halt, dummy and
  * alignment cycles (4016/4017 only in the halt cycle; the others read open bus instead), reads
  * happen on odd cycles, OAM writes on even ones, and a DMC fetch takes a read cycle from a
  * running OAM copy. Each cycle the unit decides from its state at the start of the cycle, then
  * the DMC's tick can schedule or cancel a fetch, then the register writes land. */
class NesApuPlugin(timing: NesTiming = NesTiming.ntsc) extends FiberPlugin {
  val pins = during setup new Area { val audio = out(UInt(16 bits)).setName("audio") }
  /** The mixed output, `NesMixer`'s scale, from the state at the start of the cycle. */
  def audio = pins.audio
  val logic = during setup new Area {
    val bus = host[BusService]
    val ctrl = host[IrqService]
    val retainers = Seq(retains(bus.busLock), retains(ctrl.irqLock))
    awaitBuild()
    val slave = bus.newSlave(OrMapping(Seq(SizeMapping(0x4000, 0x16), SingleMapping(Nes.FRAME_COUNTER))))
    val master = bus.newMaster(priority = 1)
    val irqLine = ctrl.request(Nes.Irq.IRQ)
    retainers.foreach(_.release())
    val open = host[OpenBusPlugin].data
    val ints = host[NesInterruptsPlugin]
    val cpu = host[SequencerService].master

    val odd = RegInit(False)
    odd := !odd
    val frameCounter = Reg(UInt(16 bits)).init(0).simPublic()
    val counterReset = Reg(UInt(3 bits)).init(NesReference.powerUpCounterReset)
    val fiveStep, irqInhibit, frameIrq = RegInit(False)
    val length = Vec.fill(4)(Reg(UInt(8 bits)).init(0))
    val halt = Vec.fill(4)(RegInit(False))
    // Halt flags and length loads apply after the next cycle's tick, as in `NesReference`.
    val newHalt = Vec.fill(4)(RegInit(False))
    val loadValid = Vec.fill(4)(RegInit(False))
    val loadValue, loadBefore = Vec.fill(4)(Reg(UInt(8 bits)).init(0))
    val enabled = Reg(Bits(4 bits)).init(0)

    // ---- Frame counter and length counters: the tick ----------------------------------------
    val resetNow = counterReset === 1
    val count = resetNow ? U(0, 16 bits) | frameCounter + 1
    val Seq(step1, step2, step3, step4) = timing.steps
    val step5 = timing.fifthStep
    val half = (resetNow && fiveStep) || (fiveStep ? (count === step2 || count === step5) | (count === step2 || count === step4))
    val irqSet = !fiveStep && count >= step4 - 1 && count <= step4 + 1 && !irqInhibit
    val lengthClocked = Vec(length.zip(halt).map { case (l, h) => (half && l =/= 0 && !h) ? (l - 1) | l })
    val lengthTicked = Vec((0 until 4).map(i => (loadValid(i) && lengthClocked(i) === loadBefore(i)) ? loadValue(i) | lengthClocked(i)))
    halt := newHalt
    loadValid.foreach(_ := False)
    val irqTicked = frameIrq || irqSet
    frameCounter := ((fiveStep ? (count === step5 + 1) | (count === step4 + 1)) ? U(0, 16 bits) | count)
    counterReset := (counterReset =/= 0) ? (counterReset - 1) | counterReset
    length := lengthTicked

    // ---- Tone channels: the tick ----------------------------------------------------------------
    val quarter = (resetNow && fiveStep) || count === step1 || count === step2 || count === step3 || count === (fiveStep ? U(step5) | U(step4))
    val pulse1 = new PulseUnit(first = true)
    val pulse2 = new PulseUnit(first = false)
    val triangle = new TriangleUnit
    val noise = new NoiseUnit(timing.noisePeriods)
    pulse1.tick(quarter, half)
    pulse2.tick(quarter, half)
    triangle.tick(quarter, lengthClocked(2) =/= 0)
    noise.tick(quarter)

    // ---- DMC ----------------------------------------------------------------------------------
    val dmcRates = Vec(timing.dmcRates.map(r => U(r - 1, 9 bits)))
    val dmc = new Area {
      val irq, irqEnable, loop = RegInit(False)
      val period = Reg(UInt(9 bits)).init(timing.dmcRates(0) - 1)
      val timer = Reg(UInt(9 bits)).init(timing.dmcRates(0) - 1)
      val sampleAddress = Reg(UInt(16 bits)).init(0)
      val sampleLength = Reg(UInt(12 bits)).init(0)
      val address = Reg(UInt(16 bits)).init(0)
      val bytes = Reg(UInt(12 bits)).init(0).simPublic()
      val buffer, shift = Reg(UInt(8 bits)).init(0)
      val bufferEmpty, silence = RegInit(True)
      val bits = Reg(UInt(4 bits)).init(8)
      val level = Reg(UInt(7 bits)).init(0).simPublic()
      val startDelay, disableDelay = Reg(UInt(2 bits)).init(0)

      // The tick, in the model's order: the disable delay, the start delay, then the timer.
      val stop = disableDelay === 1
      val bytesTicked = stop ? U(0, 12 bits) | bytes
      when(disableDelay =/= 0) { disableDelay := disableDelay - 1 }
      when(stop) { bytes := 0 }
      val startDelayed = startDelay === 1 && bufferEmpty && bytesTicked =/= 0
      when(startDelay =/= 0) { startDelay := startDelay - 1 }
      val clock = timer === 0
      timer := clock ? period | (timer - 1)
      val refill = clock && bits === 1 && !bufferEmpty
      when(clock) {
        when(!silence) {
          when(shift(0)) { when(level <= 125) { level := level + 2 } } otherwise { when(level >= 2) { level := level - 2 } }
        }
        shift := shift |>> 1
        bits := bits - 1
        when(bits === 1) {
          bits := 8
          silence := bufferEmpty
          when(!bufferEmpty) { shift := buffer; bufferEmpty := True }
        }
      }
      val start = startDelayed || (refill && bytesTicked =/= 0)
    }

    // ---- DMA unit -----------------------------------------------------------------------------
    val dma = new Area {
      val needHalt, needDummy, dmcRun, abort, oamRun, active = RegInit(False)
      val page = Reg(UInt(8 bits)).init(0)
      val count = Reg(UInt(10 bits)).init(0)
      val value = Reg(UInt(8 bits)).init(0)
      val cpuReads = cpu.cmd.valid && !cpu.cmd.write
      val inputPort = cpu.cmd.address === Nes.JOY1 || cpu.cmd.address === Nes.FRAME_COUNTER
      val get = odd // this cycle's number is odd

      // The flags after this cycle's decision; the DMC's tick and the registers act on top.
      val halt1, dummy1, dmcRun1, abort1 = Bool()
      halt1 := needHalt; dummy1 := needDummy; dmcRun1 := dmcRun; abort1 := abort
      def step(): Unit = {
        when(abort) { dmcRun1 := False; abort1 := False; dummy1 := False; halt1 := False }
          .elsewhen(needHalt) { halt1 := False }
          .elsewhen(needDummy) { dummy1 := False }
      }
      val dmcFetch, oamGet, oamPut = CombInit(False)
      master.cmd.valid := False
      master.cmd.write := False
      master.cmd.address := cpu.cmd.address
      master.cmd.data := value
      def idleRead() = master.cmd.address := 0x5000 // open bus, no side effects
      val enter = !active && needHalt && cpuReads
      when(!active) {
        when(needHalt && cpuReads) {
          master.cmd.valid := True
          halt1 := False
          when(abort && dmcRun && !oamRun) { dmcRun1 := False; abort1 := False; idleRead() }
            .otherwise { abort1 := False; active := True; count := 0 }
        }
      } elsewhen (!(dmcRun || oamRun)) {
        active := False
      } otherwise {
        master.cmd.valid := True
        step()
        when(get && dmcRun && !needHalt && !needDummy) {
          dmcFetch := True
          master.cmd.address := dmc.address
        } elsewhen (get && oamRun) {
          oamGet := True
          master.cmd.address := page @@ count(8 downto 1)
          value := master.rdata
          count := count + 1
        } elsewhen (!get && oamRun && count(0)) {
          oamPut := True
          master.cmd.write := True
          master.cmd.address := 0x2004
          count := count + 1
          when(count === 511) { oamRun := False }
        } otherwise {
          when(inputPort) { idleRead() }
        }
      }
      // The DMC's tick: a disable drops a fetch that has not halted the CPU yet, or makes one
      // under way give up; a refill schedules one.
      val halt2, dummy2, dmcRun2, abort2 = Bool()
      halt2 := halt1; dummy2 := dummy1; dmcRun2 := dmcRun1; abort2 := abort1
      when(dmc.stop && dmcRun1) {
        when(halt1) { dmcRun2 := False; dummy2 := False; halt2 := False } otherwise { abort2 := True }
      }
      when(dmc.start) { dmcRun2 := True; dummy2 := True; halt2 := True }
      needHalt := halt2; needDummy := dummy2; dmcRun := dmcRun2; abort := abort2
      // Entering the halt clears an abort, even one the tick just raised (as the model does).
      when(enter && !(abort && dmcRun && !oamRun)) { abort := False }
      when(dmcFetch) { dmcRun := False; abort := False }
    }

    // ---- Registers ----------------------------------------------------------------------------
    val a = slave.cmd.address
    val data = slave.cmd.data
    import slave.{reads, writes}
    slave.rdata := open
    when(a === Nes.APU_STATUS) {
      slave.rdata := (dmc.irq ## irqTicked ## open(5) ## (dmc.bytesTicked =/= 0) ## lengthTicked.map(_ =/= 0).asBits()).asUInt
    }

    val irqNext, dmcIrqNext = Bool()
    dmcIrqNext := dmc.irq
    irqNext := irqTicked && !reads(Nes.APU_STATUS)
    for (i <- 0 until 4) {
      val control = Seq(0x4000, 0x4004, 0x4008, 0x400C)(i)
      when(writes(control)) { newHalt(i) := (if (i == 2) data(7) else data(5)) }
      when(writes(control + 3) && enabled(i)) {
        loadValid(i) := True
        loadValue(i) := Vec(Nes.lengthTable.map(n => U(n, 8 bits)))(data(7 downto 3))
        loadBefore(i) := lengthTicked(i)
      }
      when(writes(Nes.APU_STATUS) && !data(i)) { length(i) := 0 }
    }
    for ((unit, base) <- Seq((pulse1.write _, 0x4000), (pulse2.write _, 0x4004), (triangle.write _, 0x4008), (noise.write _, 0x400C)); reg <- 0 until 4)
      when(writes(base + reg)) { unit(reg, data.asBits) }
    when(writes(0x4010)) {
      dmc.irqEnable := data(7)
      dmc.loop := data(6)
      dmc.period := dmcRates(data(3 downto 0))
      when(!data(7)) { dmcIrqNext := False }
    }
    when(writes(0x4011)) { dmc.level := data(6 downto 0) }
    when(writes(0x4012)) { dmc.sampleAddress := U"11" @@ data @@ U(0, 6 bits) }
    when(writes(0x4013)) { dmc.sampleLength := data @@ U"0001" }
    when(writes(Nes.OAMDMA)) { dma.oamRun := True; dma.page := data; dma.needHalt := True }
    when(writes(Nes.APU_STATUS)) {
      enabled := data(3 downto 0).asBits
      dmcIrqNext := False
      when(!data(4)) {
        when(dmc.disableDelay === 0) { dmc.disableDelay := odd ? U(3, 2 bits) | U(2, 2 bits) }
      } elsewhen (dmc.bytesTicked === 0) {
        dmc.address := dmc.sampleAddress
        dmc.bytes := dmc.sampleLength
        dmc.startDelay := odd ? U(3, 2 bits) | U(2, 2 bits)
      }
    }
    when(writes(Nes.FRAME_COUNTER)) {
      fiveStep := data(7)
      irqInhibit := data(6)
      when(data(6)) { irqNext := False }
      counterReset := odd ? U(4, 3 bits) | U(3, 3 bits)
    }
    // A DMC fetch completes (no CPU access happens in that cycle).
    when(dma.dmcFetch && dmc.bytesTicked =/= 0) {
      dmc.buffer := master.rdata
      dmc.bufferEmpty := False
      dmc.address := (dmc.address === 0xFFFF) ? U(0x8000, 16 bits) | (dmc.address + 1)
      dmc.bytes := dmc.bytesTicked - 1
      when(dmc.bytesTicked === 1) {
        when(dmc.loop) { dmc.address := dmc.sampleAddress; dmc.bytes := dmc.sampleLength }
          .elsewhen(dmc.irqEnable) { dmcIrqNext := True }
      }
    }
    // The reset button, as `NesReference.reset` (after this cycle's register writes).
    when(ints.resetTaken) {
      dmcIrqNext := False
      when(dmc.disableDelay === 0) { dmc.disableDelay := 2 }
      enabled := 0
      length.foreach(_ := 0)
      irqInhibit := False
      irqNext := False
      counterReset := NesReference.powerUpCounterReset
    }
    frameIrq := irqNext
    val mixer = new MixerUnit
    audio := mixer.mix(pulse1.output(length(0) =/= 0), pulse2.output(length(1) =/= 0), triangle.output, noise.output(length(3) =/= 0), dmc.level)
    dmc.irq := dmcIrqNext
    // The CPU samples its IRQ line at the end of the cycle: the flags as this cycle leaves them.
    irqLine := irqNext || dmcIrqNext
  }
}

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
  // A negated target never mutes: below 0 (pulse 1 with shift 0) it is 0, not a 12-bit wrap.
  val muted = period < 8 || (!sweepNegate && target > 0x7FF)
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

class NoiseUnit(periods: Seq[Int] = NesNoise.periods.toSeq) extends Area {
  val envelope = new EnvelopeUnit
  val mode = RegInit(False)
  val period = Reg(UInt(12 bits)).init(periods(0) - 1)
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
    case 2 => mode := v(7); period := Vec(periods.map(p => U(p - 1, 12 bits)))(v(3 downto 0).asUInt)
    case 3 => envelope.start := True
    case _ =>
  }
  def output(playing: Bool): UInt = (playing && !lfsr(0)) ? envelope.output | U(0, 4 bits)
}

/** `NesMixer` as two ROMs; the big one reads like block RAM (`MidCycleRead`). */
class MixerUnit extends Area {
  require(NesMixer.pulse.last + NesMixer.tnd.last < 65536)
  val pulseRom = Mem(UInt(16 bits), NesMixer.pulse.map(v => U(v, 16 bits)).toSeq)
  val tndRom = Mem(UInt(16 bits), NesMixer.tnd.map(v => U(v, 16 bits)).toSeq)
  def mix(pulse1: UInt, pulse2: UInt, triangle: UInt, noise: UInt, dmc: UInt): UInt = {
    val (wt, wn, wd) = NesMixer.tndWeights
    val l = (triangle * U(wt, 7 bits)).resize(13) + (noise * U(wn, 6 bits)).resize(13) + (dmc * U(wd, 6 bits)).resize(13)
    pulseRom.readAsync(pulse1 +^ pulse2) + MidCycleRead(tndRom, l)
  }
}
