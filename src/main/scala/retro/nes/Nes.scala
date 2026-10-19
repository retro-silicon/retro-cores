package retro.nes

import retro.core._
import retro.r2a03._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

/** NES (NTSC) constants shared by the RTL plugins and `NesReference`.
  *
  * Timing convention, the same as the model's: each CPU cycle the PPU runs two dots, the APU
  * ticks, the bus access happens (seeing that state, writes landing on top), the PPU runs its
  * third dot, and the CPU samples NMI and IRQ as they are at the end of the cycle. */
object Nes {
  val OAMDMA = 0x4014; val APU_STATUS = 0x4015; val JOY1 = 0x4016; val FRAME_COUNTER = 0x4017
  /** APU length counter loads, indexed by the top 5 bits of the 4003/4007/400B/400F write. */
  val lengthTable = Seq(10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
                        12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30)

  /** The 2A03 plugins plus the NES ones, in one host. `NesOpenBusPlugin` claims every address
    * no other slave does, so the CPU has no external bus port. `romDecoder`: see `SequencerPlugin`
    * (the simulations use it). */
  def plugins(cart: NesCartSpec, withTrace: Boolean = true, romDecoder: Boolean = false): Seq[Hostable] =
    Ricoh2A03.plugins(withTrace, external = false, romDecoder = romDecoder) ++ Seq(
    new NesInterruptsPlugin,
    new NesOpenBusPlugin,
    new RamPlugin("ram", SizeMapping(0x0000, 0x2000), 0x800), // 0800-1FFF mirror it
    new NesPpuPlugin,
    new NesApuPlugin,
    new NesCartridgePlugin(cart))

  def apply(cart: NesCartSpec, withTrace: Boolean = true, romDecoder: Boolean = false): RetroCore =
    RetroCore(plugins(cart, withTrace, romDecoder)).setDefinitionName("Nes")
}

/** The CPU's NMI and IRQ inputs: the PPU drives `nmi`, the APU `irq`. `Ricoh2A03.InterruptPlugin`
  * finds this plugin and wires its lines to them. */
class NesInterruptsPlugin extends FiberPlugin with Ricoh2A03.InterruptLines {
  val early = during setup new Area { val nmi, irq, reset = Bool() }
  def nmi = early.nmi
  def irq = early.irq
  override def reset = early.reset
  /** The reset button (`reset_button` pin): a pulse is held as a request until the CPU's next
    * opcode fetch takes it; `resetTaken` is high in that fetch cycle, for the APU. */
  val logic = during setup new Area {
    val seq = host[SequencerPlugin]
    awaitBuild()
    val button = in(Bool()).setName("reset_button")
    val request = RegInit(False)
    when(button) { request := True }
    val resetTaken = seq.fetchFire && request
    when(resetTaken) { request := False }
    early.reset := request
  }
}

/** The data bus keeps the last value driven on it: reads nothing answers return it, and the APU
  * and controller registers leave some bits undriven. Every completed access, the DMA's too,
  * refreshes it. */
class NesOpenBusPlugin extends FiberPlugin {
  val logic = during setup new Area {
    val bus = host[BusPlugin]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(SizeMapping(0x0000, 0x10000), priority = -1)
    retainer.release()
    val latch = Reg(UInt(8 bits)).init(0)
    slave.rdata := latch
    val b = bus.logic.get
    when(b.cmd.valid && !b.stall) { latch := b.cmd.write ? b.cmd.data | b.rdata }
  }
}

/** The 2A03's APU and DMA unit, mirroring `NesReference`: the frame counter with its IRQ, the
  * length counters (4015 reads them), the two pulses, the triangle and the noise channel
  * (`NesApuUnits.scala`), the DMC (its timer, sample buffer, memory reader, output level and
  * IRQ), the mixer (`audio`, one sample per cycle from the state at the start of the cycle),
  * OAM DMA (4014), and controller port 1 (4016, `buttons` pins: A B Select Start Up Down Left
  * Right in bits 0-7, registered, so a change shows a cycle later). Reads of the other registers in 4000-4017 are open bus.
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
class NesApuPlugin extends FiberPlugin {
  var buttons: Bits = null
  /** The mixed output, `NesMixer`'s scale, from the state at the start of the cycle. */
  var audio: UInt = null
  val logic = during setup new Area {
    val bus = host[BusService]
    val seq = host[SequencerPlugin]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(SizeMapping(0x4000, 0x18))
    val master = bus.newMaster(priority = 1)
    retainer.release()
    val open = host[NesOpenBusPlugin].logic.get.latch
    val ints = host[NesInterruptsPlugin]
    val cpu = seq.logic.get.master
    buttons = in(Bits(8 bits)).setName("joypad1")
    // The pins are registered, as asynchronous inputs should be. It also keeps the bus read data,
    // and so most of the design, off the inputs, which Verilator re-evaluates on every edge.
    val pad = RegNext(buttons).init(0)
    audio = out(UInt(16 bits)).setName("audio")

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
    val strobe = RegInit(False)
    val shift = Reg(UInt(8 bits)).init(0)

    // ---- Frame counter and length counters: the tick ----------------------------------------
    val resetNow = counterReset === 1
    val count = resetNow ? U(0, 16 bits) | frameCounter + 1
    val half = (resetNow && fiveStep) || (fiveStep ? (count === 14913 || count === 37281) | (count === 14913 || count === 29829))
    val irqSet = !fiveStep && count >= 29828 && count <= 29830 && !irqInhibit
    val lengthClocked = Vec(length.zip(halt).map { case (l, h) => (half && l =/= 0 && !h) ? (l - 1) | l })
    val lengthTicked = Vec((0 until 4).map(i => (loadValid(i) && lengthClocked(i) === loadBefore(i)) ? loadValue(i) | lengthClocked(i)))
    halt := newHalt
    loadValid.foreach(_ := False)
    val irqTicked = frameIrq || irqSet
    frameCounter := ((fiveStep ? (count === 37282) | (count === 29830)) ? U(0, 16 bits) | count)
    counterReset := (counterReset =/= 0) ? (counterReset - 1) | counterReset
    length := lengthTicked

    // ---- Tone channels: the tick ----------------------------------------------------------------
    val quarter = (resetNow && fiveStep) || count === 7457 || count === 14913 || count === 22371 || count === (fiveStep ? U(37281) | U(29829))
    val pulse1 = new PulseUnit(first = true)
    val pulse2 = new PulseUnit(first = false)
    val triangle = new TriangleUnit
    val noise = new NoiseUnit
    pulse1.tick(quarter, half)
    pulse2.tick(quarter, half)
    triangle.tick(quarter, lengthClocked(2) =/= 0)
    noise.tick(quarter)

    // ---- DMC ----------------------------------------------------------------------------------
    val dmcRates = Vec(Seq(428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54).map(r => U(r - 1, 9 bits)))
    val dmc = new Area {
      val irq, irqEnable, loop = RegInit(False)
      val period = Reg(UInt(9 bits)).init(427)
      val timer = Reg(UInt(9 bits)).init(427)
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
    def reads(address: Int) = slave.readFire && a === address
    def writes(address: Int) = slave.writeFire && a === address
    val bit = strobe ? pad(0) | shift(0)
    slave.rdata := open
    when(a === Nes.APU_STATUS) {
      slave.rdata := (dmc.irq ## irqTicked ## open(5) ## (dmc.bytesTicked =/= 0) ## lengthTicked.map(_ =/= 0).asBits()).asUInt
    }
    when(a === Nes.JOY1) { slave.rdata := open(7 downto 5) @@ U(0, 4 bits) @@ bit.asUInt }

    val irqNext, dmcIrqNext = Bool()
    dmcIrqNext := dmc.irq
    irqNext := irqTicked && !reads(Nes.APU_STATUS)
    when(reads(Nes.JOY1) && !strobe) { shift := U(1, 1 bits) @@ shift(7 downto 1) }
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
    when(writes(Nes.JOY1)) {
      strobe := data(0)
      when(data(0)) { shift := pad.asUInt }
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
    when(ints.logic.get.resetTaken) {
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
    ints.irq := irqNext || dmcIrqNext || host[NesCartridgePlugin].logic.get.irq
  }
}

/** What an iNES cartridge needs in hardware: sizes, mapper (0 NROM, 1 MMC1, 3 CNROM, 4 MMC3)
  * and mirroring. */
case class NesCartSpec(prgBytes: Int, chrBytes: Int, chrRam: Boolean, mapper: Int, verticalMirroring: Boolean) {
  require(Seq(0, 1, 3, 4).contains(mapper), s"mapper $mapper is not supported")
  require(prgBytes % 0x4000 == 0 && isPow2(chrBytes))
  require(mapper != 4 || isPow2(prgBytes), "MMC3 PRG must be a power of 2")
}
object NesCartSpec {
  def of(c: NesCartridge) = NesCartSpec(c.prg.length, c.chr.length, c.chrRam, c.mapper, c.verticalMirroring)
}

/** The cartridge: PRG ROM at 8000-FFFF, 8 KB of PRG RAM at 6000 (test ROMs report there), CHR
  * ROM or RAM for the PPU, the nametable mirroring, and the mapper's registers as
  * `NesCartridge` models them: CNROM's CHR bank (any write to 8000-FFFF), or MMC1's shift
  * register, control, CHR and PRG banks. Memories are `simPublic`: a sim loads an image with
  * `load` instead of recompiling per ROM. */
class NesCartridgePlugin(val spec: NesCartSpec) extends FiberPlugin {
  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val prgSlave = bus.newSlave(SizeMapping(0x8000, 0x8000))
    val ramSlave = bus.newSlave(SizeMapping(0x6000, 0x2000))
    retainer.release()

    // ---- Mapper registers, and their values once this cycle's access has written them -------
    val chrBank0, chrBank1 = Reg(UInt(5 bits)).init(0)
    val prgBank = Reg(UInt(4 bits)).init(0)
    val control = Reg(Bits(5 bits)).init(0x0C)
    val chrBank0After, chrBank1After = UInt(5 bits)
    val prgBankAfter = UInt(4 bits)
    val controlAfter = Bits(5 bits)
    chrBank0After := chrBank0; chrBank1After := chrBank1; prgBankAfter := prgBank; controlAfter := control
    val d = prgSlave.cmd.data
    if (spec.mapper == 3) when(prgSlave.writeFire) { chrBank0After := d(1 downto 0).resize(5) }
    if (spec.mapper == 1) {
      val shift = Reg(UInt(5 bits)).init(0)
      val shiftCount = Reg(UInt(3 bits)).init(0)
      val wrote = RegNext(prgSlave.writeFire).init(False)
      val full = shift | (d(0).asUInt.resize(5) |<< shiftCount)
      when(prgSlave.writeFire) {
        when(d(7)) {
          shift := 0; shiftCount := 0
          controlAfter := control | B"01100"
        } elsewhen (!wrote) {
          shift := full; shiftCount := shiftCount + 1
          when(shiftCount === 4) {
            shift := 0; shiftCount := 0
            switch(prgSlave.cmd.address(14 downto 13)) {
              is(0) { controlAfter := full.asBits }
              is(1) { chrBank0After := full }
              is(2) { chrBank1After := full }
              is(3) { prgBankAfter := full(3 downto 0) }
            }
          }
        }
      }
    }
    chrBank0 := chrBank0After; chrBank1 := chrBank1After; prgBank := prgBankAfter; control := controlAfter

    // MMC3: bank select, R0-R7, mirroring, and the scanline counter's latch and enable. Writes
    // to C001 (reload) and E000 (acknowledge) act on the counter state in `ppuBus`.
    val mmc3 = (spec.mapper == 4) generate new Area {
      val bankSelect = Reg(Bits(8 bits)).init(0)
      val banks = Vec(Seq(0, 2, 4, 5, 6, 7, 0, 1).map(b => Reg(UInt(8 bits)).init(b)))
      val horizontal, irqEnabled = RegInit(False)
      val irqLatch = Reg(UInt(8 bits)).init(0)
      val bankSelectAfter = CombInit(bankSelect)
      val banksAfter = CombInit(banks)
      val horizontalAfter = CombInit(horizontal)
      val irqLatchAfter = CombInit(irqLatch)
      val irqEnabledAfter = CombInit(irqEnabled)
      val reloadWrite, ackWrite = CombInit(False)
      when(prgSlave.writeFire) {
        switch(prgSlave.cmd.address(14 downto 13) ## prgSlave.cmd.address(0)) {
          is(0) { bankSelectAfter := d.asBits }
          is(1) { banksAfter(bankSelect(2 downto 0).asUInt) := d }
          is(2) { horizontalAfter := d(0) }
          is(4) { irqLatchAfter := d }
          is(5) { reloadWrite := True }
          is(6) { irqEnabledAfter := False; ackWrite := True }
          is(7) { irqEnabledAfter := True }
          default {}
        }
      }
      bankSelect := bankSelectAfter; banks := banksAfter; horizontal := horizontalAfter
      irqLatch := irqLatchAfter; irqEnabled := irqEnabledAfter
      /** A12 as last driven, dots since it fell (saturating), the counter, its reload request
        * and the IRQ, as `NesCartridge` keeps them. */
      val a12 = RegInit(False)
      val lowDots = Reg(UInt(4 bits)).init(0)
      val counter = Reg(UInt(8 bits)).init(0)
      val reload, irq = RegInit(False)
    }
    /** The cartridge's IRQ output as this cycle leaves it (MMC3), set by `ppuBus`. */
    val irq = Bool()
    if (spec.mapper != 4) irq := False

    val prg = Mem.fill(spec.prgBytes)(UInt(8 bits)).setName("cart_prg").simPublic()
    val pa = prgSlave.cmd.address
    val prgBank16 = UInt(4 bits)
    prgBank16 := pa(14).asUInt.resize(4)
    if (spec.mapper == 1) switch(control(3 downto 2)) {
      is(2) { prgBank16 := pa(14) ? prgBank | U(0, 4 bits) }
      is(3) { prgBank16 := pa(14) ? U(15, 4 bits) | prgBank }
      default { prgBank16 := (prgBank(3 downto 1) ## pa(14)).asUInt }
    }
    val banks = spec.prgBytes >> 14
    val prgIndex =
      if (spec.mapper == 4) {
        val banks8 = spec.prgBytes >> 13
        val swap = mmc3.bankSelect(6)
        val bank = UInt(8 bits)
        switch(pa(14 downto 13)) {
          is(0) { bank := swap ? U(banks8 - 2, 8 bits) | mmc3.banks(6) }
          is(1) { bank := mmc3.banks(7) }
          is(2) { bank := swap ? mmc3.banks(6) | U(banks8 - 2, 8 bits) }
          default { bank := U(banks8 - 1, 8 bits) }
        }
        bank.resize(log2Up(banks8)) @@ pa(12 downto 0)
      } else if (banks == 1) pa(13 downto 0)
      else if (isPow2(banks)) prgBank16.resize(log2Up(banks)) @@ pa(13 downto 0)
      else Vec((0 until 16).map(b => U(b % banks, log2Up(banks) bits)))(prgBank16) @@ pa(13 downto 0)
    prgSlave.rdata := prg.readAsync(prgIndex)
    val prgRam = Mem.fill(0x2000)(UInt(8 bits)).setName("cart_prg_ram").simPublic()
    prgRam.init(Seq.fill(0x2000)(U(0, 8 bits)))
    ramSlave.rdata := prgRam.readAsync(ramSlave.cmd.address(12 downto 0))
    prgRam.write(ramSlave.cmd.address(12 downto 0), ramSlave.cmd.data, ramSlave.writeFire)

    val chr = Mem.fill(spec.chrBytes)(UInt(8 bits)).setName("cart_chr").simPublic()
    if (spec.chrRam) chr.init(Seq.fill(spec.chrBytes)(U(0, 8 bits)))
    /** CHR index of a PPU address; `after` uses the registers as this cycle's access leaves them. */
    def chrIndex(a: UInt, after: Boolean = false): UInt = {
      val (bank0, bank1, ctrl) = if (after) (chrBank0After, chrBank1After, controlAfter) else (chrBank0, chrBank1, control)
      val i = spec.mapper match {
        case 3 => bank0(1 downto 0) @@ a(12 downto 0)
        case 1 => ctrl(4) ? ((a(12) ? bank1 | bank0) @@ a(11 downto 0)) | (bank0(4 downto 1) @@ a(12 downto 0))
        case 4 =>
          val (select, regs) = if (after) (mmc3.bankSelectAfter, mmc3.banksAfter) else (mmc3.bankSelect, mmc3.banks)
          val slot = a(12 downto 10) ^ (select(7) ## B"00").asUInt
          val bank = slot(2) ? regs((U"1" @@ slot(1 downto 0)) - 2) | (regs(U"00" @@ slot(1)) (7 downto 1) @@ slot(0 downto 0))
          bank @@ a(9 downto 0)
        case _ => a(12 downto 0)
      }
      i.resize(log2Up(spec.chrBytes))
    }
    def ciramIndex(a: UInt, after: Boolean): UInt = {
      val mirroring =
        if (spec.mapper == 1) (if (after) controlAfter else control)(1 downto 0)
        else if (spec.mapper == 4) B"1" ## (if (after) mmc3.horizontalAfter else mmc3.horizontal)
        else B(if (spec.verticalMirroring) 2 else 3, 2 bits)
      val i = UInt(11 bits)
      switch(mirroring) {
        is(0) { i := U"0" @@ a(9 downto 0) }
        is(1) { i := U"1" @@ a(9 downto 0) }
        is(2) { i := a(10 downto 0) }
        default { i := (a(11) ## a(9 downto 0)).asUInt }
      }
      i
    }
  }

  /** A read port on CHR at a PPU address (0000-1FFF). Call it from a build phase. With
    * `afterAccess`, a bank switch written by this cycle's CPU access already applies. */
  def chrRead(address: UInt, afterAccess: Boolean = false): UInt = logic.chr.readAsync(logic.chrIndex(address, afterAccess))
  /** The PPU's write port, used only with CHR RAM. */
  def chrWrite(address: UInt, data: UInt, enable: Bool): Unit = if (spec.chrRam) logic.chr.write(logic.chrIndex(address), data, enable)
  /** The PPU's bus as MMC3's scanline counter sees it, from the PPU's build: for each of the
    * cycle's three dots whether it drove an address and that address's A12, and the same for
    * the CPU access's two possible events (the 2007 access, then v after it when not
    * rendering), which come between the second and third dot along with mapper writes. */
  def ppuBus(dots: Seq[(Bool, Bool)], access: Seq[(Bool, Bool)]): Unit = if (spec.mapper == 4) {
    val m = logic.mmc3
    case class S(a12: Bool, low: UInt, counter: UInt, reload: Bool, irq: Bool)
    def event(s: S, valid: Bool, high: Bool, latch: UInt, enabled: Bool): S = {
      val clock = valid && high && !s.a12 && s.low >= NesCartridge.a12Filter
      val next = (s.counter === 0 || s.reload) ? latch | (s.counter - 1)
      S(valid ? high | s.a12,
        (valid && !high && s.a12) ? U(0, 4 bits) | s.low,
        clock ? next | s.counter,
        !clock && s.reload,
        s.irq || (clock && next === 0 && enabled))
    }
    def dot(s: S, e: (Bool, Bool), latch: UInt, enabled: Bool) =
      event(s.copy(low = (s.low === 15) ? s.low | (s.low + 1)), e._1, e._2, latch, enabled)
    val start = S(m.a12, m.lowDots, m.counter, m.reload, m.irq)
    val s2 = dot(dot(start, dots(0), m.irqLatch, m.irqEnabled), dots(1), m.irqLatch, m.irqEnabled)
    val written = s2.copy(counter = m.reloadWrite ? U(0, 8 bits) | s2.counter, reload = m.reloadWrite || s2.reload, irq = !m.ackWrite && s2.irq)
    val accessed = access.foldLeft(written)((s, e) => event(s, e._1, e._2, m.irqLatchAfter, m.irqEnabledAfter))
    val s3 = dot(accessed, dots(2), m.irqLatchAfter, m.irqEnabledAfter)
    m.a12 := s3.a12; m.lowDots := s3.low; m.counter := s3.counter; m.reload := s3.reload; m.irq := s3.irq
    logic.irq := s3.irq
  }

  /** A nametable address (2000-3FFF) to an index into the console's 2 KB of VRAM. With
    * `afterAccess`, a mirroring change written by this cycle's CPU access already applies. */
  def ciramIndex(address: UInt, afterAccess: Boolean = false): UInt = logic.ciramIndex(address, afterAccess)

  /** Copies an iNES image's PRG and CHR into the simulated cartridge. */
  def load(c: NesCartridge): Unit = {
    for (i <- c.prg.indices) logic.prg.setBigInt(i, c.prg(i))
    if (!spec.chrRam) for (i <- c.chr.indices) logic.chr.setBigInt(i, c.chr(i))
  }
}
