package retro.gb

import retro.core._
import retro.sm83._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc._

import scala.collection.mutable.ArrayBuffer

/** Sequential update code over hardware values, for logic that is easiest to state the way a
  * software model states it: a `UCell` or `BCell` holds the current value of some state (a
  * register's output at first), `:=` replaces it, and `If` runs both branches and muxes every
  * cell either branch changed. Reading a cell after writing it sees the new value, as in the
  * model, so a block of model code translates line by line. Each assignment and merge is a
  * fresh wire, which keeps the generated expressions small. Cells made inside a branch are local
  * to it. */
class SeqLogic {
  abstract class Cell { def value: Data; def restore(d: Data): Unit; def mux(c: Bool, t: Data, f: Data): Unit }
  final class UCell(val width: Int, var v: UInt) extends Cell {
    def :=(e: UInt): Unit = { val w = UInt(width bits); w := e.resize(width); v = w }
    def :=(i: Int): Unit = v = U(i, width bits)
    def value = v
    def restore(d: Data) = v = d.asInstanceOf[UInt]
    def mux(c: Bool, t: Data, f: Data) = { val w = UInt(width bits); w := (c ? t.asInstanceOf[UInt] | f.asInstanceOf[UInt]); v = w }
  }
  final class BCell(var v: Bool) extends Cell {
    def :=(e: Bool): Unit = { val w = Bool(); w := e; v = w }
    def :=(b: Boolean): Unit = v = Bool(b)
    def value = v
    def restore(d: Data) = v = d.asInstanceOf[Bool]
    def mux(c: Bool, t: Data, f: Data) = { val w = Bool(); w := (c ? t.asInstanceOf[Bool] | f.asInstanceOf[Bool]); v = w }
  }
  private val cells = ArrayBuffer[Cell]()
  private val commits = ArrayBuffer[() => Unit]()

  def u(width: Int, init: UInt): UCell = { val c = new UCell(width, init.resize(width)); cells += c; c }
  def u(width: Int, init: Int): UCell = u(width, U(init, width bits))
  def b(init: Bool): BCell = { val c = new BCell(init); cells += c; c }
  def b(init: Boolean): BCell = b(Bool(init))
  /** A cell over a register that takes the cell's final value. */
  def reg(width: Int, init: BigInt, name: String): UCell = {
    val r = Reg(UInt(width bits)).init(init).setName(name).simPublic()
    val c = u(width, r)
    commits += (() => r := c.v)
    c
  }
  def regB(init: Boolean, name: String): BCell = {
    val r = Reg(Bool()).init(init).setName(name).simPublic()
    val c = b(r)
    commits += (() => r := c.v)
    c
  }
  def commit(): Unit = commits.foreach(_())

  def If(c: Bool)(t: => Unit): Unit = IfElse(c)(t)(())
  def IfElse(c: Bool)(t: => Unit)(f: => Unit): Unit = {
    val n = cells.length
    val before = cells.map(_.value).toArray
    t
    val afterT = cells.take(n).map(_.value).toArray
    cells.remove(n, cells.length - n)
    for (i <- 0 until n) cells(i).restore(before(i))
    f
    val afterF = cells.take(n).map(_.value).toArray
    cells.remove(n, cells.length - n)
    for (i <- 0 until n) {
      if (afterT(i) eq afterF(i)) cells(i).restore(afterT(i))
      else cells(i).mux(c, afterT(i), afterF(i))
    }
  }

  implicit def cellU(c: UCell): UInt = c.v
  implicit def cellB(c: BCell): Bool = c.v
}

/** The DMG APU, FF10-FF3F: `GbApuReference` in hardware, line by line, with the same timing. Each
  * M-cycle the frame sequencer steps on the timer counter's bit 12 edges, then two 2 MHz ticks
  * run, then the CPU's access sees the ticked state and a write lands on it; a DIV write that
  * drops bit 12 steps the sequencer once more.
  *
  * `audio` is the mixed output after each of the two ticks: left, right, left, right, each a
  * signed sum over channels of (15 - 2 * sample) * (NR50 volume + 1). */
class GbApuPlugin(postBoot: Boolean = true) extends PeripheralPlugin(SizeMapping(0xFF10, 0x30)) {
  val pins = during setup new Area { val audio = out(Vec.fill(4)(SInt(10 bits))).setName("apu_audio").simPublic() }
  def audio = pins.audio
  def body(slave: BusSlave, irq: Seq[Bool]): Area = new Area {
    import GbApuReference._
    val timer = host[GbTimerPlugin]
    val busCmd = host[BusService].access
    val pcNow = host[RegFileService].read(Sm83Regs.PC)
    val audio = pins.audio

    val s = new SeqLogic
    import s._

    def regInit(r: Int) = if (!postBoot) 0 else Gb.PostBoot.apu.getOrElse(r, 0)
    val regs = (0 until 0x17).map(r => reg(8, regInit(r), f"apu_nr$r%02x"))
    val wave = (0 until 16).map(k => reg(8, 0, s"apu_wave_$k"))
    val lastAddress = Reg(UInt(16 bits)).init(0)
    when(busCmd.valid) { lastAddress := busCmd.address }

    val enabled = regB(postBoot, "apu_enabled")
    val active = (0 until 4).map(i => regB(postBoot && i == 0, s"apu_active_$i"))
    val samples = (0 until 4).map(i => reg(4, 0, s"apu_sample_$i"))
    val divDivider = reg(3, if (postBoot) 1 else 0, "apu_divDivider")
    val lfDiv = reg(1, 1, "apu_lfDiv")
    val skipDiv = reg(2, SkipInactive, "apu_skipDiv")

    val sweepCountdown = reg(3, 0, "apu_sweepCountdown")
    val sweepCalcCountdown = reg(3, 0, "apu_sweepCalcCountdown")
    val sweepCalcReload = reg(3, 0, "apu_sweepCalcReload")
    val sweepAddend = reg(11, 0, "apu_sweepAddend")
    val shadowLength = reg(11, 0, "apu_shadowLength")
    val unshiftedSweep = regB(false, "apu_unshiftedSweep")
    val sweepInstantDone = regB(false, "apu_sweepInstantDone")
    val restartHold = reg(2, 0, "apu_restartHold")
    val completedAddend = reg(11, 0, "apu_completedAddend")

    class Envelope(n: String, lockedInit: Boolean) {
      val locked = regB(lockedInit, s"${n}_locked")
      val clock = regB(false, s"${n}_clock")
      val shouldLock = regB(false, s"${n}_shouldLock")
      def set(value: Bool, increase: Bool, volume: UInt): Unit = If(clock =/= value) {
        IfElse(value) {
          clock := True
          shouldLock := volume === 0xF && increase || volume === 0 && !increase
        } {
          clock := False
          locked := locked || shouldLock
        }
      }
      def clear(): Unit = { locked := false; clock := false; shouldLock := false }
    }
    class Square(i: Int) {
      val n = s"apu_square$i"
      val boot = postBoot && i == 0
      val length = reg(7, if (boot) 0x40 else 0, s"${n}_length")
      val volume = reg(4, 0, s"${n}_volume")
      val volumeCountdown = reg(3, 0, s"${n}_volumeCountdown")
      val index = reg(3, if (boot) 2 else 0, s"${n}_index")
      val high = regB(false, s"${n}_high")
      val suppressed = regB(false, s"${n}_suppressed")
      val countdown = reg(13, if (boot) 12 else 0x1FFF, s"${n}_countdown")
      val period = reg(11, if (boot) 0x7C1 else 0, s"${n}_period")
      val lengthEnabled = regB(false, s"${n}_lengthEnabled")
      val envelope = new Envelope(s"${n}_envelope", boot)
      val didTick = regB(false, s"${n}_didTick")
      val justReloaded = regB(false, s"${n}_justReloaded")
      def clear(): Unit = {
        length := 0; volume := 0; volumeCountdown := 0; index := 0; high := false; suppressed := false; countdown := 0
        period := 0; lengthEnabled := false; envelope.clear(); didTick := false; justReloaded := false
      }
    }
    val square = Seq(new Square(0), new Square(1))

    val waveEnable = regB(false, "apu_waveEnable")
    val waveLength = reg(9, 0, "apu_waveLength")
    val waveShift = reg(3, 0, "apu_waveShift")
    val wavePeriod = reg(11, 0, "apu_wavePeriod")
    val waveLengthEnabled = regB(false, "apu_waveLengthEnabled")
    val waveCountdown = reg(12, 0, "apu_waveCountdown")
    val waveIndex = reg(5, 0, "apu_waveIndex")
    val waveByte = reg(8, 0, "apu_waveByte")
    val waveJustRead = regB(false, "apu_waveJustRead")
    val wavePulsed = regB(false, "apu_wavePulsed")
    val waveBuggedRead = reg(2, 0, "apu_waveBuggedRead")

    val noiseLength = reg(7, 0, "apu_noiseLength")
    val noiseVolume = reg(4, 0, "apu_noiseVolume")
    val noiseVolumeCountdown = reg(3, 0, "apu_noiseVolumeCountdown")
    val lfsr = reg(15, 0, "apu_lfsr")
    val narrow = regB(false, "apu_narrow")
    val noiseCountdown = reg(6, 0, "apu_noiseCountdown")
    val noiseCounter = reg(14, 0, "apu_noiseCounter")
    val noiseLengthEnabled = regB(false, "apu_noiseLengthEnabled")
    val alignment = reg(2, 0, "apu_alignment")
    val lfsrSample = regB(false, "apu_lfsrSample")
    val didStepCounter = regB(false, "apu_didStepCounter")
    val countdownReloaded = regB(false, "apu_countdownReloaded")
    val delayedStart = reg(3, 0, "apu_delayedStart")
    val noiseEnvelope = new Envelope("apu_noiseEnvelope", false)
    val noiseCounterActive = regB(false, "apu_noiseCounterActive")
    val noiseBackgroundActive = regB(false, "apu_noiseBackgroundActive")
    val startedWithDacOff = regB(false, "apu_startedWithDacOff")

    // ---- The model's helpers ----------------------------------------------------------------
    def dacOn(i: Int): Bool = i match {
      case 0 => regs(0x02).v(7 downto 3) =/= 0
      case 1 => regs(0x07).v(7 downto 3) =/= 0
      case 2 => waveEnable.v
      case 3 => regs(0x11).v(7 downto 3) =/= 0
    }
    def update(i: Int, value: UInt): Unit = If(dacOn(i)) { samples(i) := value }
    def mix(): Seq[SInt] = {
      def side(volume: UInt, route: Int => Bool) = (0 until 4)
        .map { i =>
          val a = S(15, 6 bits) - (samples(i).v << 1).resize(6).asSInt
          (dacOn(i) && route(i)) ? (a * (volume.resize(5) + 1).asSInt).resize(10) | S(0, 10 bits)
        }
        .reduce(_ + _)
      val nr50 = regs(0x14).v
      val nr51 = regs(0x15).v
      Seq(side(nr50(6 downto 4), i => nr51(4 + i)), side(nr50(2 downto 0), i => nr51(i)))
    }
    val dutyBits = Vec(U(0x80, 8 bits), U(0x81, 8 bits), U(0xE1, 8 bits), U(0x7E, 8 bits))
    def nrx1(i: Int) = regs(if (i == 0) 0x01 else 0x06)
    def nrx2(i: Int) = regs(if (i == 0) 0x02 else 0x07)
    def updateSquare(i: Int): Unit = If(!square(i).suppressed) { update(i, square(i).high ? square(i).volume.v | U(0, 4 bits)) }
    def updateWave(): Unit = update(2, ((waveIndex.v(0) ? waveByte.v(3 downto 0) | waveByte.v(7 downto 4)) >> waveShift.v).resize(4))
    def waveAt(k: UInt): UInt = Vec(wave.map(_.v))(k.resize(4))
    def setWave(k: UInt, value: UInt): Unit = for (j <- 0 until 16) If(k.resize(4) === j) { wave(j) := value }

    def nrx2Glitch(volume: UCell, value: UInt, old: UInt, countdown: UCell, env: Envelope): Unit = {
      def glitch(value: UInt, old: UInt): Unit = {
        If(env.clock) { countdown := value(2 downto 0) }
        val shouldTick = b((value(2 downto 0) =/= 0 && old(2 downto 0) === 0 && !env.locked) ||
          (value(3 downto 0) === 8 && old(3 downto 0) === 8 && !env.locked))
        If(value(3) =/= old(3)) {
          IfElse(value(3)) {
            IfElse(old(2 downto 0) === 0 && !env.locked) { volume := volume ^ 0xF } { volume := U(0xE, 4 bits) - volume }
            shouldTick := false
          } { volume := U(0, 4 bits) - volume }
        }
        IfElse(shouldTick) { volume := (value(3) ? (volume + 1) | (volume - 1)) } {
          If(value(2 downto 0) === 0 && env.clock) { env.set(False, False, U(0, 4 bits)) }
        }
      }
      // The DMG goes through FF on the way.
      glitch(U(0xFF, 8 bits), old)
      glitch(value, U(0xFF, 8 bits))
    }

    def tickSquareEnvelope(i: Int): Unit = {
      val sq = square(i)
      sq.envelope.set(False, False, U(0, 4 bits))
      If(!sq.envelope.locked && nrx2(i).v(2 downto 0) =/= 0) {
        sq.volume := (nrx2(i).v(3) ? (sq.volume + 1) | (sq.volume - 1))
        If(active(i)) { updateSquare(i) }
      }
    }
    def tickNoiseEnvelope(): Unit = {
      noiseEnvelope.set(False, False, U(0, 4 bits))
      If(!noiseEnvelope.locked && regs(0x11).v(2 downto 0) =/= 0) {
        noiseVolume := (regs(0x11).v(3) ? (noiseVolume + 1) | (noiseVolume - 1))
        If(active(3)) { update(3, lfsr.v(0) ? noiseVolume.v | U(0, 4 bits)) }
      }
    }

    def sweepCalculationDone(): Unit = {
      // APU bug: the frequency is checked after adding the sweep delta twice.
      If(restartHold === 0) { shadowLength := square(0).period }
      If(regs(0x00).v(3)) { sweepAddend := sweepAddend ^ 0x7FF }
      If(shadowLength.v.resize(12) + sweepAddend.v.resize(12) > 0x7FF && !regs(0x00).v(3)) { active(0) := false; update(0, U(0, 4 bits)) }
      completedAddend := sweepAddend
    }

    def triggerSweepCalculation(duringDivWrite: Bool): Unit = {
      val nr10 = regs(0x00).v
      If(nr10(6 downto 4) =/= 0 && sweepCountdown === 7) {
        If(nr10(2 downto 0) =/= 0) { square(0).period := sweepAddend + shadowLength + nr10(3).asUInt }
        If(restartHold === 0) { sweepAddend := square(0).period.v >> nr10(2 downto 0) }
        sweepCalcCountdown := nr10(2 downto 0)
        sweepCalcReload := (duringDivWrite ? U(1, 3 bits) | (U(1, 3 bits) + lfDiv.v))
        unshiftedSweep := nr10(2 downto 0) === 0
        sweepCountdown := nr10(6 downto 4) ^ 7
        If(sweepCalcCountdown === 0) { sweepInstantDone := true }
      }
    }

    def divEvent(duringDivWrite: Bool): Unit = If(enabled) {
      IfElse(skipDiv === SkipSkip) { skipDiv := SkipSkipped } {
        IfElse(skipDiv === SkipSkipped) { skipDiv := SkipInactive } { divDivider := divDivider + 1 }
        If(divDivider === 7) {
          for (sq <- square) If(!sq.envelope.clock) { sq.volumeCountdown := sq.volumeCountdown - 1 }
          If(!noiseEnvelope.clock) { noiseVolumeCountdown := noiseVolumeCountdown - 1 }
        }
        for (i <- 0 to 1) If(square(i).envelope.clock) { tickSquareEnvelope(i) }
        If(noiseEnvelope.clock) { tickNoiseEnvelope() }
        If(divDivider.v(0)) {
          for (i <- 0 to 1) {
            val sq = square(i)
            If(sq.lengthEnabled && sq.length =/= 0) {
              sq.length := sq.length - 1
              If(sq.length === 0) { active(i) := false; update(i, U(0, 4 bits)) }
            }
          }
          If(waveLengthEnabled && waveLength =/= 0) {
            waveLength := waveLength - 1
            If(waveLength === 0) { active(2) := false; update(2, U(0, 4 bits)) }
          }
          If(noiseLengthEnabled && noiseLength =/= 0) {
            noiseLength := noiseLength - 1
            If(noiseLength === 0) { active(3) := false; update(3, U(0, 4 bits)) }
          }
        }
        If(divDivider.v(1 downto 0) === 3) {
          sweepCountdown := sweepCountdown + 1
          triggerSweepCalculation(duringDivWrite)
        }
      }
    }

    def divSecondaryEvent(): Unit = If(enabled) {
      for (i <- 0 to 1) {
        val sq = square(i)
        If(active(i) && sq.volumeCountdown === 0) {
          sq.volumeCountdown := nrx2(i).v(2 downto 0)
          sq.envelope.set(sq.volumeCountdown =/= 0, nrx2(i).v(3), sq.volume)
        }
      }
      If(active(3) && noiseVolumeCountdown === 0) {
        noiseVolumeCountdown := regs(0x11).v(2 downto 0)
        noiseEnvelope.set(noiseVolumeCountdown =/= 0, regs(0x11).v(3), noiseVolume)
      }
    }

    def updateLfsr(): Unit = {
      lfsrSample := lfsr.v(0)
      If(active(3)) { update(3, lfsrSample ? noiseVolume.v | U(0, 4 bits)) }
    }
    def stepLfsr(): Unit = {
      val newHigh = !(lfsr.v(0) ^ lfsr.v(1))
      val shifted = (lfsr.v >> 1).resize(15)
      val highMask = narrow ? U(0x4040, 15 bits) | U(0x4000, 15 bits)
      lfsr := (newHigh ? (shifted | highMask) | (shifted & ~highMask))
      updateLfsr()
    }
    def counterBit(counter: UInt, bit: UInt): Bool = counter.resize(16)(bit.resize(4))

    def tick(): Unit = {
      If(waveBuggedRead =/= 0) {
        waveBuggedRead := waveBuggedRead - 1
        If(waveBuggedRead === 0) {
          waveByte := waveAt(lastAddress(3 downto 0))
          If(active(2)) { updateWave() }
        }
      }
      val startNoise = b(false)
      If(delayedStart =/= 0) {
        delayedStart := delayedStart - 1
        If(delayedStart === 0) { startNoise := true }
      }
      lfDiv := lfDiv ^ 1
      alignment := alignment + 1

      val sweepCycles = u(3, (lfDiv.v === 0).asUInt)
      IfElse(sweepCalcReload > sweepCycles) { sweepCalcReload := sweepCalcReload - sweepCycles; sweepCycles := 0 } {
        If(sweepCalcReload =/= 0 && sweepCalcCountdown === 0 && sweepInstantDone) { sweepCalculationDone() }
        sweepInstantDone := false
        sweepCycles := sweepCycles - sweepCalcReload
        sweepCalcReload := 0
      }
      If(sweepCalcCountdown =/= 0 && (regs(0x00).v(2 downto 0) =/= 0 || unshiftedSweep)) {
        IfElse(sweepCalcCountdown > sweepCycles) { sweepCalcCountdown := sweepCalcCountdown - sweepCycles } {
          sweepCalcCountdown := 0
          sweepCalculationDone()
        }
      }
      If(restartHold =/= 0) { restartHold := restartHold - 1 }

      for (i <- 0 to 1) If(active(i)) {
        val sq = square(i)
        IfElse(sq.countdown === 0) {
          sq.countdown := ((sq.period.v ^ 0x7FF).resize(13) << 1) + 1
          sq.index := sq.index + 1
          sq.high := dutyBits(nrx1(i).v(7 downto 6))(sq.index.v)
          sq.suppressed := false
          sq.didTick := true
          updateSquare(i)
          sq.justReloaded := true
        } {
          sq.justReloaded := false
          sq.countdown := sq.countdown - 1
        }
      }

      waveJustRead := false
      IfElse(active(2)) {
        IfElse(waveCountdown === 0) {
          waveCountdown := wavePeriod.v ^ 0x7FF
          waveIndex := waveIndex + 1
          waveByte := waveAt(waveIndex.v >> 1)
          updateWave()
          waveJustRead := true
        } { waveCountdown := waveCountdown - 1 }
      } {
        If(waveEnable && wavePulsed) {
          IfElse(waveCountdown === 0) {
            waveCountdown := wavePeriod.v ^ 0x7FF
            waveBuggedRead := 1
          } { waveCountdown := waveCountdown - 1 }
          If(waveCountdown === 0) { waveBuggedRead := 2 }
        }
      }

      If(noiseCounterActive || noiseBackgroundActive) {
        val nr43 = regs(0x12).v
        val divisor = (nr43(2 downto 0) === 0) ? U(2, 6 bits) | (nr43(2 downto 0) << 2).resize(6)
        If(noiseCountdown === 0) { noiseCountdown := divisor }
        IfElse(noiseCountdown === 1) {
          noiseCountdown := divisor
          val oldBit = counterBit(noiseCounter, nr43(7 downto 4))
          noiseCounter := noiseCounter + 1
          didStepCounter := true
          val newBit = counterBit(noiseCounter, nr43(7 downto 4))
          If(newBit && !oldBit && active(3)) { stepLfsr() }
          countdownReloaded := true
        } {
          noiseCountdown := noiseCountdown - 1
          countdownReloaded := false
        }
      }
      If(startNoise) { write(0x13, regs(0x13).v | 0x80, False) }
    }

    def lengthGlitch(value: UInt, lengthEnabled: Bool, length: UCell, reload: Int, i: Int): Unit = {
      // APU glitch: enabling the length while the frame sequencer's next step clocks lengths ticks it once.
      If(value(6) && !lengthEnabled && divDivider.v(0) && length =/= 0) {
        length := length - 1
        If(length === 0) {
          IfElse(value(7)) { length := reload } { active(i) := false; update(i, U(0, 4 bits)) }
        }
      }
    }

    def prepareNoiseStart(): Unit = {
      val nr43 = regs(0x12).v
      noiseCounterActive := regs(0x11).v(7 downto 3) =/= 0
      val wasStartedWithDacOff = b(startedWithDacOff)
      startedWithDacOff := !noiseCounterActive
      val divisor = u(4, nr43(2 downto 0))
      val wasBackground = b(noiseBackgroundActive)
      noiseBackgroundActive := true
      val instantStep = b(false)
      val div1Glitch = b(false)

      IfElse(divisor > 1 && noiseCountdown === 1) { noiseCounter := noiseCounter + 1 } {
        If(noiseCountdown === 2 && alignment === 0 && active(3)) {
          IfElse(divisor === 0) { divisor := 8 } {
            If(divisor === 1) {
              If(!didStepCounter) { div1Glitch := true }
              val oldBit = counterBit(noiseCounter, nr43(7 downto 4))
              noiseCounter := noiseCounter + 1
              If(counterBit(noiseCounter, nr43(7 downto 4)) && !oldBit) { instantStep := true }
            }
          }
        }
      }
      noiseCountdown := ((divisor === 0) ? U(6, 6 bits) | ((divisor.v << 2).resize(6) + 6))
      IfElse(alignment.v(0)) {
        IfElse(divisor === 0) { noiseCountdown := noiseCountdown + 1 } {
          IfElse(alignment.v(1)) {
            IfElse(divisor === 1 && !active(3)) { noiseCountdown := noiseCountdown + 1 } { noiseCountdown := noiseCountdown - 3 }
          } {
            noiseCountdown := noiseCountdown - 1
            If(divisor === 1 && active(3)) { noiseCountdown := noiseCountdown - 4 }
          }
        }
      } {
        If(divisor =/= 0) {
          IfElse(alignment.v(1)) { noiseCountdown := noiseCountdown - 2 } {
            IfElse(divisor > 1) { noiseCountdown := noiseCountdown - 4 } {
              If(divisor === 1 && active(3) && nr43(7 downto 4) === 0) { noiseCountdown := noiseCountdown - 4 }
            }
          }
        }
      }
      IfElse(divisor > 1) { If(!noiseCounterActive && alignment === 0) { noiseCountdown := noiseCountdown + 4 } } {
        If(wasBackground && !active(3) && alignment === 0) {
          IfElse(divisor === 0) { If(wasStartedWithDacOff) { noiseCountdown := noiseCountdown + 28 } } {
            noiseCountdown := noiseCountdown - 4
          }
        }
      }
      If(div1Glitch) { noiseCountdown := noiseCountdown - 4 }
      lfsr := ((divisor === 0 && active(3) && alignment === 3) ? U(0x55, 15 bits) | U(0, 15 bits))
      If(instantStep) { stepLfsr() }
    }

    /** SameBoy's `nr43_write` for pre-CGB-D models. */
    def nr43Write(value: UInt): Unit = {
      narrow := value(3)
      val old = regs(0x12).v
      regs(0x12) := value
      If(old(7 downto 4) =/= value(7 downto 4)) {
        val effective = countdownReloaded ? (noiseCounter.v | (noiseCounter.v - 1)) | noiseCounter.v
        val oldBit = counterBit(effective, old(7 downto 4))
        val glitchValue = (old & 0x7F) | (value & 0x80)
        val glitchBit = counterBit(effective, glitchValue(7 downto 4))
        val newBit = counterBit(effective, value(7 downto 4))
        IfElse(oldBit === newBit && newBit =/= glitchBit) { If(!newBit) { stepLfsr() } } {
          IfElse(!oldBit && newBit) {
            val previousNarrow = b(narrow)
            narrow := true
            stepLfsr()
            narrow := previousNarrow
            If(value(7 downto 4) <= 2 && glitchBit && !effective(3)) {
              stepLfsr()
              val keep = narrow ? U(0x4040, 15 bits) | U(0x4000, 15 bits)
              val from = narrow ? U(0x2020, 15 bits) | U(0x2000, 15 bits)
              lfsr := (lfsr.v & ~keep) | ((lfsr.v & from) << 1).resize(15)
            }
          } {
            If(value(7 downto 4) <= 2 && !glitchBit && !newBit && !oldBit && effective(3)) { stepLfsr() }
          }
        }
      }
    }

    def nr10Glitch(value: UInt): Unit = {
      If(!(sweepCalcReload === 1 && lfDiv === 0) && !(sweepCalcReload > 1) &&
        sweepCalcCountdown =/= 0 && regs(0x00).v(2 downto 0) === 0 && lfDiv =/= 0) {
        sweepCalcCountdown := sweepCalcCountdown - 1
        If(sweepCalcCountdown <= 1) { sweepCalcCountdown := 0; sweepCalculationDone() }
      }
    }

    def clearState(): Unit = {
      enabled := false
      for (i <- 0 until 4) { active(i) := false; samples(i) := 0 }
      divDivider := 0; lfDiv := 0; skipDiv := SkipInactive
      sweepCountdown := 0; sweepCalcCountdown := 0; sweepCalcReload := 0; sweepAddend := 0; shadowLength := 0
      unshiftedSweep := false; sweepInstantDone := false; restartHold := 0; completedAddend := 0
      square.foreach(_.clear())
      waveEnable := false; waveLength := 0; waveShift := 0; wavePeriod := 0; waveLengthEnabled := false; waveCountdown := 0
      waveIndex := 0; waveByte := 0; waveJustRead := false; wavePulsed := false; waveBuggedRead := 0
      noiseLength := 0; noiseVolume := 0; noiseVolumeCountdown := 0; lfsr := 0; narrow := false; noiseCountdown := 0
      noiseCounter := 0; noiseLengthEnabled := false; alignment := 0; lfsrSample := false; didStepCounter := false
      countdownReloaded := false; delayedStart := 0; noiseEnvelope.clear(); noiseCounterActive := false
      noiseBackgroundActive := false; startedWithDacOff := false
    }
    def powerOn(counter: UInt): Unit = {
      clearState()
      lfDiv := 1
      waveShift := 4
      // APU glitch: turning the APU on while bit 12 is set skips the first frame sequencer step.
      If(counter(12)) { skipDiv := SkipSkip; divDivider := 1 }
      square(0).countdown := 0x1FFF
      square(1).countdown := 0x1FFF
      enabled := true
    }
    def powerOff(): Unit = {
      for (i <- 0 until 4) update(i, U(0, 4 bits))
      clearState()
      for (k <- 0 until 0x17) regs(k) := 0
      enabled := false
    }

    /** A write to FF10 + `r`, `r` below 0x20 (wave RAM is `writeWave`). */
    def write(r: Int, value: UInt, duringDivWrite: Bool): Unit = {
      val blocked = r != 0x16 && r != 0x01 && r != 0x06 && r != 0x0B && r != 0x10
      If(if (blocked) enabled.v else True) {
        var stored = value
        r match {
          case 0x14 | 0x15 =>
          case 0x16 =>
            val oldLengths = Seq(square(0).length.v, square(1).length.v, waveLength.v, noiseLength.v)
            // It sees the counter one M-cycle later (Gambatte's ch2_init_reset_env_counter_timing_5).
            IfElse(value(7) && !enabled) { powerOn(timer.tickedCounter.get + 4) } { If(!value(7) && enabled) { powerOff() } }
            If(value(7)) {
              square(0).length := oldLengths(0); square(1).length := oldLengths(1)
              waveLength := oldLengths(2); noiseLength := oldLengths(3)
            }
          case 0x00 =>
            If(sweepCalcCountdown =/= 0 || sweepCalcReload =/= 0) { nr10Glitch(value) }
            regs(0x00) := value
            If(shadowLength.v.resize(12) + completedAddend.v.resize(12) + 1 > 0x7FF && !value(3)) { active(0) := false; update(0, U(0, 4 bits)) }
            triggerSweepCalculation(duringDivWrite)
          case 0x01 | 0x06 =>
            square(if (r == 0x06) 1 else 0).length := U(0x40, 7 bits) - value(5 downto 0).resize(7)
            stored = enabled ? value | (value & 0x3F)
          case 0x02 | 0x07 =>
            val i = if (r == 0x07) 1 else 0
            val sq = square(i)
            IfElse(value(7 downto 3) === 0) { regs(r) := value; active(i) := false; update(i, U(0, 4 bits)) } {
              If(active(i)) {
                nrx2Glitch(sq.volume, value, regs(r).v, sq.volumeCountdown, sq.envelope)
                updateSquare(i)
              }
            }
          case 0x03 | 0x08 =>
            val sq = square(if (r == 0x08) 1 else 0)
            sq.period := sq.period.v(10 downto 8) @@ value
            If(sq.justReloaded) { sq.countdown := ((sq.period.v ^ 0x7FF).resize(13) << 1) + 1 }
          case 0x04 | 0x09 =>
            val i = if (r == 0x09) 1 else 0
            val sq = square(i)
            val wasActive = active(i).v
            If(!value(7) && active(i) && regs(r).v(2 downto 0) === 7 && value(2 downto 0) =/= 7 && sq.countdown.v(0) &&
              sq.didTick && (sq.countdown.v >> 1) === (sq.period.v ^ 0x7FF).resize(12)) {
              sq.index := sq.index - 1
              sq.suppressed := false
            }
            sq.period := value(2 downto 0) @@ sq.period.v(7 downto 0)
            If(sq.justReloaded) { sq.countdown := ((sq.period.v ^ 0x7FF).resize(13) << 1) + 1 }
            If(value(7)) {
              sq.envelope.locked := false
              sq.envelope.clock := false
              sq.didTick := false
              // Timing quirk: if already active, the sound starts 2 ticks earlier.
              sq.countdown := ((sq.period.v ^ 0x7FF).resize(13) << 1) + (active(i) ? U(4, 13 bits) | U(6, 13 bits)) - lfDiv.v
              sq.volume := nrx2(i).v(7 downto 4)
              If(active(i)) { updateSquare(i) }
              sq.volumeCountdown := nrx2(i).v(2 downto 0)
              If(nrx2(i).v(7 downto 3) =/= 0 && !active(i)) { active(i) := true; update(i, U(0, 4 bits)); sq.suppressed := true }
              If(sq.length === 0) { sq.length := 0x40; sq.lengthEnabled := false }
              if (i == 0) {
                sweepInstantDone := false
                shadowLength := 0
                completedAddend := 0
                val nr10 = regs(0x00).v
                IfElse(nr10(2 downto 0) =/= 0) {
                  // APU bug: with a nonzero shift, the overflow check also happens on trigger.
                  sweepCalcCountdown := nr10(2 downto 0)
                  sweepCalcReload := ((lfDiv.v === 0) ? U(3, 3 bits) | U(2, 3 bits)) + (!wasActive).asUInt
                  unshiftedSweep := false
                  sweepAddend := sq.period.v >> nr10(2 downto 0)
                } { sweepAddend := 0 }
                restartHold := U(2, 2 bits) - lfDiv.v
                sweepCountdown := nr10(6 downto 4) ^ 7
              }
            }
            lengthGlitch(value, sq.lengthEnabled, sq.length, 0x3F, i)
            sq.lengthEnabled := value(6)
          case 0x0A =>
            waveEnable := value(7)
            If(!waveEnable) {
              wavePulsed := false
              If(active(2)) {
                IfElse(waveCountdown === 0) { waveByte := waveAt(pcNow(3 downto 0)) } {
                  If(waveJustRead) { waveByte := wave(0xA) }
                }
              }
              active(2) := false
              update(2, U(0, 4 bits))
            }
          case 0x0B => waveLength := U(0x100, 9 bits) - value.resize(9)
          case 0x0C =>
            waveShift := value(6 downto 5).muxListDc(Seq(0 -> U(4, 3 bits), 1 -> U(0, 3 bits), 2 -> U(1, 3 bits), 3 -> U(2, 3 bits)))
            If(active(2)) { updateWave() }
          case 0x0D =>
            wavePeriod := wavePeriod.v(10 downto 8) @@ value
            If(waveBuggedRead === 1) { waveCountdown := wavePeriod.v ^ 0x7FF }
          case 0x0E =>
            wavePeriod := value(2 downto 0) @@ wavePeriod.v(7 downto 0)
            If(value(7)) {
              wavePulsed := true
              // DMG bug: retriggering 1 cycle before the APU reads wave RAM corrupts it.
              If(active(2) && waveCountdown === 0) {
                val offset = ((waveIndex.v + 1) >> 1).resize(4)
                val bytes = wave.map(_.v)
                IfElse(offset < 4) { wave(0) := Vec(bytes)(offset) } {
                  for (k <- 0 until 4) wave(k) := Vec(bytes)((offset & 0xC) | k)
                }
              }
              waveIndex := 0
              If(active(2) && waveCountdown === 0) { waveByte := wave(0) }
              If(waveEnable) { active(2) := true; update(2, (waveByte.v(7 downto 4) >> waveShift.v).resize(4)) }
              waveCountdown := (wavePeriod.v ^ 0x7FF).resize(12) + 3
              If(waveLength === 0) { waveLength := 0x100; waveLengthEnabled := false }
            }
            lengthGlitch(value, waveLengthEnabled, waveLength, 0xFF, 2)
            waveLengthEnabled := value(6)
          case 0x10 => noiseLength := U(0x40, 7 bits) - value(5 downto 0).resize(7)
          case 0x11 =>
            IfElse(value(7 downto 3) === 0) {
              If(active(3) && regs(0x12).v(2 downto 0) =/= 0) {
                If(noiseCountdown <= 2) { noiseCounter := noiseCounter + 1 }
                noiseBackgroundActive := false
              }
              regs(r) := value
              active(3) := false
              update(3, U(0, 4 bits))
              noiseCounterActive := false
            } {
              If(active(3)) {
                nrx2Glitch(noiseVolume, value, regs(r).v, noiseVolumeCountdown, noiseEnvelope)
                update(3, lfsrSample ? noiseVolume.v | U(0, 4 bits))
              }
            }
          case 0x12 =>
            If(countdownReloaded) {
              val divisor = (value(2 downto 0) === 0) ? U(2, 6 bits) | (value(2 downto 0) << 2).resize(6)
              val extra = alignment.v.muxListDc(Seq(0 -> U(2, 6 bits), 1 -> U(1, 6 bits), 2 -> U(4, 6 bits), 3 -> U(3, 6 bits)))
              noiseCountdown := divisor + ((value(2 downto 0) === 0) ? U(0, 6 bits) | extra)
              val old = regs(0x12).v
              val oldBit = counterBit(noiseCounter, old(7 downto 4))
              val glitchBit = noiseCounter.v(7)
              val newBit = counterBit(noiseCounter, value(7 downto 4))
              If(!oldBit && newBit && glitchBit) {
                val previous = noiseCounter.v - 1
                If(counterBit(previous, old(7 downto 4)) && !counterBit(previous, value(7 downto 4)) && previous(7)) { stepLfsr() }
              }
            }
            nr43Write(U(0xFF, 8 bits))
            nr43Write(value)
          case 0x13 =>
            If(value(7)) {
              noiseEnvelope.locked := false
              noiseEnvelope.clock := false
              IfElse(alignment =/= 0) { delayedStart := 6 } {
                lfsr := 0
                prepareNoiseStart()
                noiseVolume := regs(0x11).v(7 downto 4)
                lfsrSample := false
                noiseVolumeCountdown := regs(0x11).v(2 downto 0)
                didStepCounter := alignment === 2
                If(regs(0x11).v(7 downto 3) =/= 0) { active(3) := true; update(3, U(0, 4 bits)) }
                If(noiseLength === 0) { noiseLength := 0x40; noiseLengthEnabled := false }
              }
            }
            lengthGlitch(value, noiseLengthEnabled, noiseLength, 0x3F, 3)
            noiseLengthEnabled := value(6)
          case _ =>
        }
        regs(r) := stored
      }
    }
    def writeWave(k: Int, value: UInt): Unit =
      IfElse(active(2)) { If(waveJustRead) { setWave(waveIndex.v >> 1, value) } } { wave(k) := value }

    def read(address: UInt): UInt = {
      val nr52 = U(0x70, 8 bits) | (enabled.v.asUInt << 7).resize(8) | Vec(active.map(_.v)).asBits.asUInt.resize(8)
      val mapped = (0 until 0x20).map { r =>
        r -> (if (r == 0x16) nr52 else if (r < 0x17) regs(r).v | U(readMask(r), 8 bits) else U(0xFF, 8 bits))
      } ++ (0 until 16).map(k => (0x20 + k) -> (active(2) ? (waveJustRead ? waveAt(waveIndex.v >> 1) | U(0xFF, 8 bits)) | wave(k).v))
      address(5 downto 0).muxListDc(mapped.map { case (r, v) => r -> v })
    }

    // ---- The M-cycle ----------------------------------------------------------------------
    def fall(a: UInt, b: UInt) = a(12) && !b(12)
    IfElse(fall(timer.counter, timer.tickedCounter)) { divEvent(False) } {
      If(!timer.counter(12) && timer.tickedCounter(12)) { divSecondaryEvent() }
    }
    tick()
    val tick0 = mix()
    tick()
    val tick1 = mix()
    audio(0) := tick0(0); audio(1) := tick0(1); audio(2) := tick1(0); audio(3) := tick1(1)

    slave.rdata := read(slave.cmd.address - 0xFF10)
    val offset = (slave.cmd.address - 0xFF10).resize(6)
    val data = slave.cmd.data
    If(slave.writeFire) {
      for (r <- 0 until 0x20 if r < 0x17) If(offset === r) {
        write(r, data, False)
      }
      for (k <- 0 until 16) If(offset === 0x20 + k) { writeWave(k, data) }
    }
    // A DIV write that drops bit 12 steps the frame sequencer.
    If(timer.tickedCounter(12) && !timer.nextCounter(12)) { divEvent(True) }
    commit()
  }
}
