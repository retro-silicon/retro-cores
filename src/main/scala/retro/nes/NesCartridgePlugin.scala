package retro.nes

import retro.core._
import spinal.core._
import spinal.core.fiber._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.misc.plugin._

/** What an iNES cartridge needs in hardware: sizes, mapper (`NesCartridge.mappers`),
  * mirroring, and for MMC3 which IRQ behavior (`NesCartridge.mmc3Alternate`). */
case class NesCartSpec(prgBytes: Int, chrBytes: Int, chrRam: Boolean, mapper: Int, verticalMirroring: Boolean,
                       mmc3Alternate: Boolean = false, prgRamBytes: Int = 0x2000) {
  require(prgRamBytes >= 0x2000 && isPow2(prgRamBytes))
  require(NesCartridge.mappers.contains(mapper), s"mapper $mapper is not supported")
  require(prgBytes % 0x4000 == 0 && isPow2(chrBytes))
  require(Seq(0, 1, 3).contains(mapper) || isPow2(prgBytes), s"mapper $mapper's PRG must be a power of 2")
}
object NesCartSpec {
  def of(c: NesCartridge) = NesCartSpec(c.prg.length, c.chr.length, c.chrRam, c.mapper, c.verticalMirroring, c.mmc3Alternate, c.prgRamBytes)
}

/** The cartridge: PRG ROM at 8000-FFFF, PRG RAM at 6000 (test ROMs report there), CHR ROM or
  * RAM for the PPU, and the board's mapper (`NesBoardLogic`, one class per `NesBoard`). Memories
  * are `simPublic`: a sim loads an image with `load` instead of recompiling per ROM. `image`
  * builds the ROMs in (synthesis needs them). */
class NesCartridgePlugin(val spec: NesCartSpec, image: Option[NesCartridge] = None) extends FiberPlugin {
  val logic = during setup new Area {
    val bus = host[BusService]
    val ctrl = host[IrqService]
    val retainers = Seq(retains(bus.busLock), retains(ctrl.irqLock))
    awaitBuild()
    val prgSlave = bus.newSlave(SizeMapping(0x8000, 0x8000))
    val ramSlave = bus.newSlave(SizeMapping(0x6000, 0x2000))
    val irqLine = ctrl.request(Nes.Irq.IRQ)
    retainers.foreach(_.release())

    val board = NesBoardLogic(spec, prgSlave)
    irqLine := board.irq

    val prg = Mem.fill(spec.prgBytes)(UInt(8 bits)).setName("cart_prg").simPublic()
    for (c <- image) prg.init(c.prg.map(U(_, 8 bits)).toSeq)
    prgSlave.rdata := MidCycleRead(prg, board.prgIndex)
    val prgRam = Mem.fill(spec.prgRamBytes)(UInt(8 bits)).setName("cart_prg_ram").simPublic()
    prgRam.init(Seq.fill(spec.prgRamBytes)(U(0, 8 bits)))
    val ramBanks = spec.prgRamBytes >> 13
    val ra = ramSlave.cmd.address(12 downto 0)
    val ramAddress = if (ramBanks == 1) ra else board.ramBank.resize(log2Up(ramBanks)) @@ ra
    val ramData = MidCycleRead(prgRam, ramAddress)
    val openBus = host[OpenBusPlugin].data
    ramSlave.rdata := board.ramReadable ? ramData | board.lowOff(ra, openBus, prg)
    prgRam.write(ramAddress, ramSlave.cmd.data, ramSlave.writeFire && board.ramWritable)

    val chr = Mem.fill(spec.chrBytes)(UInt(8 bits)).setName("cart_chr").simPublic()
    if (spec.chrRam) chr.init(Seq.fill(spec.chrBytes)(U(0, 8 bits)))
    else for (c <- image) chr.init(c.chr.map(U(_, 8 bits)).toSeq)
    def chrIndex(a: UInt, after: Boolean = false, dot: Boolean = false): UInt = board.chrIndex(a, after, dot).resize(log2Up(spec.chrBytes))
    def ciramIndex(a: UInt, after: Boolean): UInt = {
      val i = UInt(11 bits)
      switch(board.mirroring(after)) {
        is(0) { i := U"0" @@ a(9 downto 0) }
        is(1) { i := U"1" @@ a(9 downto 0) }
        is(2) { i := a(10 downto 0) }
        default { i := (a(11) ## a(9 downto 0)).asUInt }
      }
      i
    }
  }

  /** The PPU's two CHR ports, read synchronously (call them from the PPU's fast clock area): the
    * dot port at a PPU address (0000-1FFF), with the bank registers as this CPU cycle's access
    * leaves them when `dotAfter`; and the access port, which reads, or with CHR RAM writes, at a
    * PPU address when `enable`. The dot port sees a write the access port makes in the same
    * clock. Returns the two ports' data. */
  def chrPorts(dotAddress: UInt, dotAfter: Bool, address: UInt, data: UInt, enable: Bool, write: Bool): (UInt, UInt) = {
    val l = logic.get
    val dotIndex = dotAfter ? l.chrIndex(dotAddress, after = true, dot = true) | l.chrIndex(dotAddress, dot = true)
    val index = l.chrIndex(address)
    if (spec.chrRam) {
      val accessData = l.chr.readWriteSync(index, data, enable, write)
      val bypass = RegNext(enable && write && index === dotIndex).init(False)
      (bypass ? RegNext(data) | l.chr.readSync(dotIndex), accessData)
    } else (l.chr.readSync(dotIndex), l.chr.readSync(index, enable))
  }
  /** The PPU's bus as the mapper sees it (MMC3's scanline counter, MMC2/MMC4's latches), from
    * the PPU's fast clock area, once per dot: whether the dot drove an address and the address,
    * and when `accessPhase` (the CPU access follows this dot), the access's two possible events
    * (the 2007 access, then v after it when not rendering), which come after the mapper writes.
    * `after`: this dot comes after the CPU cycle's access, so it sees its latch and enable
    * writes. */
  def ppuBus(dot: (Bool, UInt), access: Seq[(Bool, UInt)], accessPhase: Bool, after: Bool): Unit =
    logic.board.ppuBus(dot, access, accessPhase, after)
  /** A nametable address (2000-3FFF) to an index into the console's 2 KB of VRAM. With
    * `afterAccess`, a mirroring change written by this cycle's CPU access already applies. */
  def ciramIndex(address: UInt, afterAccess: Boolean = false): UInt = logic.ciramIndex(address, afterAccess)
  /** `ciramIndex`, with `afterAccess` chosen in hardware. */
  def ciramIndex(address: UInt, afterAccess: Bool): UInt = afterAccess ? logic.ciramIndex(address, true) | logic.ciramIndex(address, false)

  /** Copies an iNES image's PRG and CHR into the simulated cartridge. */
  def load(c: NesCartridge): Unit = {
    for (i <- c.prg.indices) logic.prg.setBigInt(i, c.prg(i))
    if (!spec.chrRam) for (i <- c.chr.indices) logic.chr.setBigInt(i, c.chr(i))
  }
}

/** A board's mapper in hardware, mirroring its `NesBoard`: its registers, written by CPU writes
  * to 8000-FFFF on `slave`, and how they map addresses. As is, NROM. `after` variants use the
  * registers as this cycle's access leaves them, for the PPU's dots after the access. */
class NesBoardLogic(val spec: NesCartSpec, val slave: BusSlave) extends Area {
  val banks8 = spec.prgBytes >> 13
  val banks16 = spec.prgBytes >> 14
  def write = slave.writeFire
  def d = slave.cmd.data
  def pa = slave.cmd.address
  /** PRG index of `pa` in an 8 or 16 KB bank; 16 KB bank numbers wrap at the number of banks,
    * which need not be a power of 2 (80 KB MMC1 images). */
  def prg8(bank: UInt): UInt = bank.resize(log2Up(banks8)) @@ pa(12 downto 0)
  def prg16(bank: UInt): UInt =
    if (banks16 == 1) pa(13 downto 0)
    else if (isPow2(banks16)) bank.resize(log2Up(banks16)) @@ pa(13 downto 0)
    else Vec((0 until 1 << bank.getWidth).map(b => U(b % banks16, log2Up(banks16) bits)))(bank) @@ pa(13 downto 0)
  def prg32(bank: UInt): UInt = prg16(bank @@ pa(14))

  /** CPU address (8000-FFFF) to an index into PRG ROM. */
  def prgIndex: UInt = prg16(pa(14).asUInt)
  /** PPU address to an index into CHR, before it is cut to the CHR size. `dot`: for the PPU's dot
    * port rather than its access port. */
  def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt = a(12 downto 0)
  /** Nametable mirroring, as `NesBoard.mirroring`. */
  def mirroring(after: Boolean): Bits = B(if (spec.verticalMirroring) 2 else 3, 2 bits)
  def ramReadable: Bool = True
  def ramWritable: Bool = ramReadable
  def ramBank: UInt = U(0, 2 bits)
  /** A read of 6000-7FFF (`ra`, its low 13 bits) with the RAM disabled. */
  def lowOff(ra: UInt, openBus: UInt, prg: Mem[UInt]): UInt = openBus
  /** The IRQ output as this cycle leaves it. */
  def irq: Bool = False
  def ppuBus(dot: (Bool, UInt), access: Seq[(Bool, UInt)], accessPhase: Bool, after: Bool): Unit = {}
}

object NesBoardLogic {
  def apply(spec: NesCartSpec, slave: BusSlave): NesBoardLogic = spec.mapper match {
    case 0 => new NesBoardLogic(spec, slave)
    case 1 => new Mmc1Logic(spec, slave)
    case 2 => new UxromLogic(spec, slave)
    case 3 => new CnromLogic(spec, slave)
    case 4 => new Mmc3Logic(spec, slave)
    case 7 => new AxromLogic(spec, slave)
    case 9 => new Mmc2Logic(spec, slave, mmc4 = false)
    case 10 => new Mmc2Logic(spec, slave, mmc4 = true)
    case 11 => new ColorDreamsLogic(spec, slave)
    case 34 => new BnromLogic(spec, slave)
    case 66 => new GxromLogic(spec, slave)
    case 69 => new Fme7Logic(spec, slave)
    case 180 => new Mapper180Logic(spec, slave)
  }
}

/** `Mmc1`. */
class Mmc1Logic(spec: NesCartSpec, slave: BusSlave) extends NesBoardLogic(spec, slave) {
  val chrBank0, chrBank1 = Reg(UInt(5 bits)).init(0)
  val prgBank = Reg(UInt(4 bits)).init(0)
  val control = Reg(Bits(5 bits)).init(0x0C)
  val ramOff, lastChr1 = RegInit(False)
  val chrBank0After = CombInit(chrBank0)
  val chrBank1After = CombInit(chrBank1)
  val prgBankAfter = CombInit(prgBank)
  val controlAfter = CombInit(control)
  val shift = Reg(UInt(5 bits)).init(0)
  val shiftCount = Reg(UInt(3 bits)).init(0)
  val wrote = RegNext(write).init(False)
  val full = shift | (d(0).asUInt.resize(5) |<< shiftCount)
  when(write) {
    when(d(7)) {
      shift := 0; shiftCount := 0
      controlAfter := control | B"01100"
    } elsewhen (!wrote) {
      shift := full; shiftCount := shiftCount + 1
      when(shiftCount === 4) {
        shift := 0; shiftCount := 0
        switch(pa(14 downto 13)) {
          is(0) { controlAfter := full.asBits }
          is(1) { chrBank0After := full; lastChr1 := False }
          is(2) { chrBank1After := full; lastChr1 := True }
          is(3) { prgBankAfter := full(3 downto 0); ramOff := full(4) }
        }
      }
    }
  }
  chrBank0 := chrBank0After; chrBank1 := chrBank1After; prgBank := prgBankAfter; control := controlAfter
  /** The CHR register whose bits 2-4 drive the PRG RAM lines and PRG A18. */
  val extra = (lastChr1 && control(4)) ? chrBank1 | chrBank0

  override def prgIndex: UInt = {
    val bank16 = UInt(4 bits)
    switch(control(3 downto 2)) {
      is(2) { bank16 := pa(14) ? prgBank | U(0, 4 bits) }
      is(3) { bank16 := pa(14) ? U(15, 4 bits) | prgBank }
      default { bank16 := (prgBank(3 downto 1) ## pa(14)).asUInt }
    }
    if (banks16 == 32) prg16((extra(4) ## bank16).asUInt) else prg16(bank16) // SUROM, SXROM
  }
  override def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt = {
    val (bank0, bank1, ctrl) = if (after) (chrBank0After, chrBank1After, controlAfter) else (chrBank0, chrBank1, control)
    ctrl(4) ? ((a(12) ? bank1 | bank0) @@ a(11 downto 0)) | (bank0(4 downto 1) @@ a(12 downto 0))
  }
  override def mirroring(after: Boolean): Bits = (if (after) controlAfter else control)(1 downto 0)
  val snrom = spec.chrRam && spec.prgBytes <= 0x40000
  override val ramReadable: Bool = !ramOff && !(if (snrom) extra(4) else False)
  override def ramBank: UInt = extra(3 downto 2)
}

/** `Cnrom`. */
class CnromLogic(spec: NesCartSpec, slave: BusSlave) extends NesBoardLogic(spec, slave) {
  val chrBank = Reg(UInt(2 bits)).init(0)
  val chrBankAfter = CombInit(chrBank)
  when(write) { chrBankAfter := d(1 downto 0) }
  chrBank := chrBankAfter
  override def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt = (if (after) chrBankAfter else chrBank) @@ a(12 downto 0)
}

/** `LatchBoard`. */
class LatchLogic(spec: NesCartSpec, slave: BusSlave) extends NesBoardLogic(spec, slave) {
  val latch = Reg(UInt(8 bits)).init(0)
  val latchAfter = CombInit(latch)
  when(write) { latchAfter := d }
  latch := latchAfter
  def latch(after: Boolean): UInt = if (after) latchAfter else latch
}
/** `Uxrom`. */
class UxromLogic(spec: NesCartSpec, slave: BusSlave) extends LatchLogic(spec, slave) {
  override def prgIndex: UInt = prg16(pa(14) ? U(banks16 - 1, 8 bits) | latch)
}
/** `Mapper180`. */
class Mapper180Logic(spec: NesCartSpec, slave: BusSlave) extends LatchLogic(spec, slave) {
  override def prgIndex: UInt = prg16(pa(14) ? latch | U(0, 8 bits))
}
/** `Axrom`. */
class AxromLogic(spec: NesCartSpec, slave: BusSlave) extends LatchLogic(spec, slave) {
  override def prgIndex: UInt = prg32(latch(3 downto 0))
  override def mirroring(after: Boolean): Bits = B"0" ## latch(after)(4)
}
/** `ColorDreams`. */
class ColorDreamsLogic(spec: NesCartSpec, slave: BusSlave) extends LatchLogic(spec, slave) {
  override def prgIndex: UInt = prg32(latch(1 downto 0))
  override def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt = latch(after)(7 downto 4) @@ a(12 downto 0)
}
/** `Bnrom`. */
class BnromLogic(spec: NesCartSpec, slave: BusSlave) extends LatchLogic(spec, slave) {
  override def prgIndex: UInt = prg32(latch(6 downto 0))
}
/** `Gxrom`. */
class GxromLogic(spec: NesCartSpec, slave: BusSlave) extends LatchLogic(spec, slave) {
  override def prgIndex: UInt = prg32(latch(5 downto 4))
  override def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt = latch(after)(1 downto 0) @@ a(12 downto 0)
}

/** `Mmc3`: bank select, R0-R7, mirroring, PRG RAM control, and the scanline counter's latch and
  * enable. Writes to C001 (reload) and E000 (acknowledge) act on the counter state in `ppuBus`. */
class Mmc3Logic(spec: NesCartSpec, slave: BusSlave) extends NesBoardLogic(spec, slave) {
  val bankSelect = Reg(Bits(8 bits)).init(0)
  val banks = Vec(Seq(0, 2, 4, 5, 6, 7, 0, 1).map(b => Reg(UInt(8 bits)).init(b)))
  val horizontal, irqEnabled = RegInit(False)
  val ramOn = RegInit(True)
  val ramProtect = RegInit(False)
  val irqLatch = Reg(UInt(8 bits)).init(0)
  val bankSelectAfter = CombInit(bankSelect)
  val banksAfter = CombInit(banks)
  val horizontalAfter = CombInit(horizontal)
  val irqLatchAfter = CombInit(irqLatch)
  val irqEnabledAfter = CombInit(irqEnabled)
  val reloadWrite, ackWrite = CombInit(False)
  when(write) {
    switch(pa(14 downto 13) ## pa(0)) {
      is(0) { bankSelectAfter := d.asBits }
      is(1) { banksAfter(bankSelect(2 downto 0).asUInt) := d }
      is(2) { horizontalAfter := d(0) }
      is(3) { ramOn := d(7); ramProtect := d(6) }
      is(4) { irqLatchAfter := d }
      is(5) { reloadWrite := True }
      is(6) { irqEnabledAfter := False; ackWrite := True }
      is(7) { irqEnabledAfter := True }
      default {}
    }
  }
  bankSelect := bankSelectAfter; banks := banksAfter; horizontal := horizontalAfter
  irqLatch := irqLatchAfter; irqEnabled := irqEnabledAfter
  /** A12 as last driven, dots since it fell (saturating), the counter, its reload request and
    * the IRQ, as `Mmc3` keeps them; updated every dot, on the fast clock. */
  val counterState = new ClockingArea(RetroCore.clocks.get.fast) {
    val a12 = RegInit(False)
    val lowDots = Reg(UInt(4 bits)).init(0)
    val counter = Reg(UInt(8 bits)).init(0)
    val reload, irq = RegInit(False)
  }
  /** The IRQ as this cycle leaves it, set by `ppuBus`. */
  val irqOut = Bool()
  override def irq: Bool = irqOut

  override def prgIndex: UInt = {
    val swap = bankSelect(6)
    val bank = UInt(8 bits)
    switch(pa(14 downto 13)) {
      is(0) { bank := swap ? U(banks8 - 2, 8 bits) | banks(6) }
      is(1) { bank := banks(7) }
      is(2) { bank := swap ? banks(6) | U(banks8 - 2, 8 bits) }
      default { bank := U(banks8 - 1, 8 bits) }
    }
    prg8(bank)
  }
  override def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt = {
    val (select, regs) = if (after) (bankSelectAfter, banksAfter) else (bankSelect, banks)
    val slot = a(12 downto 10) ^ (select(7) ## B"00").asUInt
    val bank = slot(2) ? regs((U"1" @@ slot(1 downto 0)) - 2) | (regs(U"00" @@ slot(1))(7 downto 1) @@ slot(0 downto 0))
    bank @@ a(9 downto 0)
  }
  override def mirroring(after: Boolean): Bits = B"1" ## (if (after) horizontalAfter else horizontal)
  override def ramReadable: Bool = ramOn
  override def ramWritable: Bool = ramOn && !ramProtect

  override def ppuBus(dotA: (Bool, UInt), accessA: Seq[(Bool, UInt)], accessPhase: Bool, after: Bool): Unit = {
    val dot = (dotA._1, dotA._2(12))
    val access = accessA.map(e => (e._1, e._2(12)))
    val r = counterState
    case class S(a12: Bool, low: UInt, counter: UInt, reload: Bool, irq: Bool)
    def event(s: S, valid: Bool, high: Bool, latch: UInt, enabled: Bool): S = {
      val clock = valid && high && !s.a12 && s.low >= NesCartridge.a12Filter
      val next = (s.counter === 0 || s.reload) ? latch | (s.counter - 1)
      S(valid ? high | s.a12,
        (valid && !high && s.a12) ? U(0, 4 bits) | s.low,
        clock ? next | s.counter,
        !clock && s.reload,
        s.irq || (clock && next === 0 && enabled && (if (spec.mmc3Alternate) s.counter =/= 0 || s.reload else True)))
    }
    val start = S(r.a12, r.lowDots, r.counter, r.reload, r.irq)
    val ticked = start.copy(low = (start.low === 15) ? start.low | (start.low + 1))
    val dotted = event(ticked, dot._1, dot._2, after ? irqLatchAfter | irqLatch, after ? irqEnabledAfter | irqEnabled)
    val reloaded = accessPhase && reloadWrite
    val written = dotted.copy(counter = reloaded ? U(0, 8 bits) | dotted.counter, reload = reloaded || dotted.reload,
      irq = !(accessPhase && ackWrite) && dotted.irq)
    val s = access.foldLeft(written)((s, e) => event(s, e._1, e._2, irqLatchAfter, irqEnabledAfter))
    r.a12 := s.a12; r.lowDots := s.low; r.counter := s.counter; r.reload := s.reload; r.irq := s.irq
    irqOut := s.irq
  }
}

/** `Mmc2`: PRG bank, the four CHR banks (FD then FE, low table then high), mirroring; and on the
  * fast clock the two latches (True: FE) and the change waiting for the next address. */
class Mmc2Logic(spec: NesCartSpec, slave: BusSlave, mmc4: Boolean) extends NesBoardLogic(spec, slave) {
  val prgBank = Reg(UInt(4 bits)).init(0)
  val banks = Vec.fill(4)(Reg(UInt(5 bits)).init(0))
  val horizontal = RegInit(False)
  val banksAfter = CombInit(banks)
  val horizontalAfter = CombInit(horizontal)
  when(write) {
    switch(pa(14 downto 12)) {
      is(2) { prgBank := d(3 downto 0) }
      is(3, 4, 5, 6) { banksAfter((pa(14 downto 12) - 3).resize(2)) := d(4 downto 0) }
      is(7) { horizontalAfter := d(0) }
      default {}
    }
  }
  banks := banksAfter; horizontal := horizontalAfter
  val latchState = new ClockingArea(RetroCore.clocks.get.fast) {
    val latch = Vec.fill(2)(RegInit(True))
    val pending = RegInit(False)
    val pendingTable, pendingFe = RegInit(False)
  }
  /** The latches the next dot's fetch reads with, and the ones the CPU access's 2007 read does;
    * set by `ppuBus`. */
  val nextLatch, accessLatch = Vec.fill(2)(Bool())

  override def prgIndex: UInt =
    if (mmc4) prg16(pa(14) ? U(banks16 - 1, 8 bits) | prgBank.resize(8))
    else prg8((pa(14 downto 13) === 0) ? prgBank.resize(8) | (U(banks8 - 4, 8 bits) + pa(14 downto 13)))
  override def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt = {
    val b = if (after) banksAfter else banks
    val l = if (dot) nextLatch else accessLatch
    b((a(12) ## (a(12) ? l(1) | l(0))).asUInt) @@ a(11 downto 0)
  }
  override def mirroring(after: Boolean): Bits = B"1" ## (if (after) horizontalAfter else horizontal)

  override def ppuBus(dot: (Bool, UInt), access: Seq[(Bool, UInt)], accessPhase: Bool, after: Bool): Unit = {
    val r = latchState
    case class S(latch: Vec[Bool], pending: Bool, table: Bool, fe: Bool)
    // An address applies the waiting change, then may set a new one.
    def event(s: S, valid: Bool, a: UInt): S = {
      val latch = CombInit(s.latch)
      when(valid && s.pending) { latch(s.table.asUInt) := s.fe }
      val low = a(11 downto 3) === (0xFD8 >> 3) || a(11 downto 3) === (0xFE8 >> 3)
      val exact = if (!mmc4) !a(12) && a(2 downto 0) =/= 0 else False
      val hit = valid && !a(13) && low && !exact
      S(latch, hit || (s.pending && !valid), valid ? a(12) | s.table, valid ? a(5) | s.fe)
    }
    def applied(s: S): Vec[Bool] = {
      val latch = CombInit(s.latch)
      when(s.pending) { latch(s.table.asUInt) := s.fe }
      latch
    }
    val start = S(r.latch, r.pending, r.pendingTable, r.pendingFe)
    val dotted = event(start, dot._1, dot._2)
    accessLatch := applied(dotted)
    val end = access.foldLeft(dotted)((s, e) => event(s, e._1, e._2))
    r.latch := end.latch; r.pending := end.pending; r.pendingTable := end.table; r.pendingFe := end.fe
    nextLatch := applied(end)
  }
}

/** `Fme7`: the command, the registers it addresses (as this cycle's access leaves them for the
  * PPU's CHR banks and mirroring), and the IRQ counter, which counts before the access. */
class Fme7Logic(spec: NesCartSpec, slave: BusSlave) extends NesBoardLogic(spec, slave) {
  val command = Reg(UInt(4 bits)).init(0)
  val regs = Vec.fill(16)(Reg(UInt(8 bits)).init(0))
  val regsAfter = CombInit(regs)
  val counter = Reg(UInt(16 bits)).init(0)
  val irqReg = RegInit(False)
  val ticked = regs(13)(7) ? (counter - 1) | counter
  val counterAfter = CombInit(ticked)
  val irqAfter = CombInit(irqReg || (regs(13)(7) && regs(13)(0) && counter === 0))
  when(write) {
    switch(pa(14 downto 13)) {
      is(0) { command := d(3 downto 0) }
      is(1) {
        regsAfter(command) := d
        switch(command) {
          is(13) { irqAfter := False }
          is(14) { counterAfter(7 downto 0) := d }
          is(15) { counterAfter(15 downto 8) := d }
          default {}
        }
      }
      default {}
    }
  }
  regs := regsAfter; counter := counterAfter; irqReg := irqAfter
  override def irq: Bool = irqAfter

  override def prgIndex: UInt =
    prg8((pa(14 downto 13) === 3) ? U(banks8 - 1, 8 bits) | regs((U(9, 4 bits) + pa(14 downto 13)))(5 downto 0).resize(8))
  override def chrIndex(a: UInt, after: Boolean, dot: Boolean): UInt =
    (if (after) regsAfter else regs)(a(12 downto 10).resize(4)) @@ a(9 downto 0)
  override def mirroring(after: Boolean): Bits = { val m = (if (after) regsAfter else regs)(12); (!m(1)) ## m(0) }
  /** 6000-7FFF: RAM selected and enabled, RAM disabled (open bus), else a ROM bank. */
  override val ramReadable: Bool = regs(8)(7 downto 6) === 3
  override def lowOff(ra: UInt, openBus: UInt, prg: Mem[UInt]): UInt =
    (regs(8)(6) && !regs(8)(7)) ? openBus | MidCycleRead(prg, regs(8)(5 downto 0).resize(log2Up(banks8)) @@ ra)
}
