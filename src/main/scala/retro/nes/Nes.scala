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

  /** The 2A03 plugins plus the NES ones, in one host. */
  def plugins(cart: NesCartSpec, withTrace: Boolean = true): Seq[Hostable] = Ricoh2A03.plugins(withTrace) ++ Seq(
    new NesInterruptsPlugin,
    new NesOpenBusPlugin,
    new RamPlugin("ram", SizeMapping(0x0000, 0x2000), 0x800), // 0800-1FFF mirror it
    new NesPpuPlugin,
    new NesApuPlugin,
    new NesOamDmaPlugin,
    new NesCartridgePlugin(cart))

  def apply(cart: NesCartSpec, withTrace: Boolean = true): RetroCore =
    RetroCore(plugins(cart, withTrace)).setDefinitionName("Nes")
}

/** The CPU's NMI and IRQ inputs: the PPU drives `nmi`, the APU `irq`. `Ricoh2A03.InterruptPlugin`
  * finds this plugin and wires its lines to them. */
class NesInterruptsPlugin extends FiberPlugin with Ricoh2A03.InterruptLines {
  val early = during setup new Area { val nmi, irq = Bool() }
  def nmi = early.nmi
  def irq = early.irq
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

/** The 2A03's APU, as far as the CPU can see it without sound: the frame counter with its IRQ,
  * the length counters of the four channels (4015 reads them), and controller port 1 (4016,
  * `buttons` pins: A B Select Start Up Down Left Right in bits 0-7). Reads of the other
  * registers in 4000-4017 are open bus.
  *
  * Each cycle the APU ticks first and the access sees the ticked state. A 4017 write resets the
  * frame counter 3 cycles later when it lands on an even cycle, 4 on an odd one (counting
  * cycles from the first reset cycle), and with bit 7 set clocks the length counters then. */
class NesApuPlugin extends FiberPlugin {
  var buttons: Bits = null
  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(SizeMapping(0x4000, 0x18))
    retainer.release()
    val open = host[NesOpenBusPlugin].logic.get.latch
    val ints = host[NesInterruptsPlugin]
    buttons = in(Bits(8 bits)).setName("joypad1")

    val odd = RegInit(False)
    odd := !odd
    val frameCounter = Reg(UInt(16 bits)).init(0).simPublic()
    val counterReset = Reg(UInt(3 bits)).init(0)
    val fiveStep, irqInhibit, frameIrq = RegInit(False)
    val length = Vec.fill(4)(Reg(UInt(8 bits)).init(0))
    val halt = Vec.fill(4)(RegInit(False))
    val enabled = Reg(Bits(4 bits)).init(0)
    val strobe = RegInit(False)
    val shift = Reg(UInt(8 bits)).init(0)

    // The tick.
    val resetNow = counterReset === 1
    val count = resetNow ? U(0, 16 bits) | frameCounter + 1
    val half = (resetNow && fiveStep) || (fiveStep ? (count === 14913 || count === 37281) | (count === 14913 || count === 29829))
    val irqSet = !fiveStep && count >= 29828 && count <= 29830 && !irqInhibit
    val lengthTicked = Vec(length.zip(halt).map { case (l, h) => (half && l =/= 0 && !h) ? (l - 1) | l })
    val irqTicked = frameIrq || irqSet
    frameCounter := ((fiveStep ? (count === 37282) | (count === 29830)) ? U(0, 16 bits) | count)
    counterReset := (counterReset =/= 0) ? (counterReset - 1) | counterReset
    length := lengthTicked

    // The access.
    val a = slave.cmd.address
    val data = slave.cmd.data
    def reads(address: Int) = slave.readFire && a === address
    def writes(address: Int) = slave.writeFire && a === address
    val bit = strobe ? buttons(0) | shift(0)
    slave.rdata := open
    when(a === Nes.APU_STATUS) { slave.rdata := (B"0" ## irqTicked ## open(5) ## B"0" ## lengthTicked.map(_ =/= 0).asBits()).asUInt }
    when(a === Nes.JOY1) { slave.rdata := open(7 downto 5) @@ U(0, 4 bits) @@ bit.asUInt }

    val irqNext = Bool()
    irqNext := irqTicked && !reads(Nes.APU_STATUS)
    when(reads(Nes.JOY1) && !strobe) { shift := U(1, 1 bits) @@ shift(7 downto 1) }
    for (i <- 0 until 4) {
      val control = Seq(0x4000, 0x4004, 0x4008, 0x400C)(i)
      when(writes(control)) { halt(i) := (if (i == 2) data(7) else data(5)) }
      when(writes(control + 3) && enabled(i)) { length(i) := Vec(Nes.lengthTable.map(n => U(n, 8 bits)))(data(7 downto 3)) }
      when(writes(Nes.APU_STATUS) && !data(i)) { length(i) := 0 }
    }
    when(writes(Nes.APU_STATUS)) { enabled := data(3 downto 0).asBits }
    when(writes(Nes.JOY1)) {
      strobe := data(0)
      when(data(0)) { shift := buttons.asUInt }
    }
    when(writes(Nes.FRAME_COUNTER)) {
      fiveStep := data(7)
      irqInhibit := data(6)
      when(data(6)) { irqNext := False }
      counterReset := odd ? U(4, 3 bits) | U(3, 3 bits)
    }
    frameIrq := irqNext
    ints.irq := irqNext
  }
}

/** OAM DMA: a write to 4014 makes this bus master, above the CPU, halt the CPU on its next read.
  * It spends one cycle on that read (repeating it, side effects included, as the chip does),
  * one more when the halt cycle was odd, so the copy starts on an even cycle, then 256
  * read-write pairs from page `xx` to 2004. The CPU's read then completes. */
class NesOamDmaPlugin extends FiberPlugin {
  val logic = during setup new Area {
    val bus = host[BusService]
    val seq = host[SequencerPlugin]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val slave = bus.newSlave(SingleMapping(Nes.OAMDMA), priority = 1)
    val master = bus.newMaster(priority = 1)
    retainer.release()
    slave.rdata := host[NesOpenBusPlugin].logic.get.latch
    val cpu = seq.logic.get.master

    val odd = RegInit(False)
    odd := !odd
    val pending = RegInit(False)
    val page = Reg(UInt(8 bits))
    val index = Reg(UInt(8 bits))
    val data = Reg(UInt(8 bits))
    val state = RegInit(NesOamDmaState.IDLE)
    master.cmd.valid := False
    master.cmd.write := False
    master.cmd.address := cpu.cmd.address
    master.cmd.data := data
    switch(state) {
      is(NesOamDmaState.IDLE) {
        when(pending && cpu.cmd.valid && !cpu.cmd.write) {
          master.cmd.valid := True
          pending := False
          index := 0
          state := odd ? NesOamDmaState.ALIGN | NesOamDmaState.GET
        }
      }
      is(NesOamDmaState.ALIGN) {
        master.cmd.valid := True
        state := NesOamDmaState.GET
      }
      is(NesOamDmaState.GET) {
        master.cmd.valid := True
        master.cmd.address := page @@ index
        data := master.rdata
        state := NesOamDmaState.PUT
      }
      is(NesOamDmaState.PUT) {
        master.cmd.valid := True
        master.cmd.write := True
        master.cmd.address := 0x2004
        index := index + 1
        state := (index === 255) ? NesOamDmaState.IDLE | NesOamDmaState.GET
      }
    }
    when(slave.writeFire) { pending := True; page := slave.cmd.data }
  }
}

object NesOamDmaState extends SpinalEnum { val IDLE, ALIGN, GET, PUT = newElement() }

/** What an iNES cartridge needs in hardware: sizes, mapper (0 NROM, 3 CNROM) and mirroring. */
case class NesCartSpec(prgBytes: Int, chrBytes: Int, chrRam: Boolean, mapper: Int, verticalMirroring: Boolean) {
  require(mapper == 0 || mapper == 3, s"mapper $mapper is not supported")
  require(isPow2(prgBytes) && isPow2(chrBytes))
}
object NesCartSpec {
  def of(c: NesCartridge) = NesCartSpec(c.prg.length, c.chr.length, c.chrRam, c.mapper, c.verticalMirroring)
}

/** The cartridge: PRG ROM at 8000-FFFF (16 KB mirrored or 32 KB), 8 KB of PRG RAM at 6000 (test
  * ROMs report there), CHR ROM or RAM for the PPU, CNROM's CHR bank register (any write to
  * 8000-FFFF) and the nametable mirroring. Memories are `simPublic`: a sim loads an image with
  * `load` instead of recompiling per ROM. */
class NesCartridgePlugin(val spec: NesCartSpec) extends FiberPlugin {
  val logic = during setup new Area {
    val bus = host[BusService]
    val retainer = retains(bus.busLock)
    awaitBuild()
    val prgSlave = bus.newSlave(SizeMapping(0x8000, 0x8000))
    val ramSlave = bus.newSlave(SizeMapping(0x6000, 0x2000))
    retainer.release()

    val prg = Mem.fill(spec.prgBytes)(UInt(8 bits)).setName("cart_prg").simPublic()
    prgSlave.rdata := prg.readAsync(prgSlave.cmd.address.resize(log2Up(spec.prgBytes)))
    val prgRam = Mem.fill(0x2000)(UInt(8 bits)).setName("cart_prg_ram").simPublic()
    prgRam.init(Seq.fill(0x2000)(U(0, 8 bits)))
    ramSlave.rdata := prgRam.readAsync(ramSlave.cmd.address(12 downto 0))
    prgRam.write(ramSlave.cmd.address(12 downto 0), ramSlave.cmd.data, ramSlave.writeFire)

    val chr = Mem.fill(spec.chrBytes)(UInt(8 bits)).setName("cart_chr").simPublic()
    if (spec.chrRam) chr.init(Seq.fill(spec.chrBytes)(U(0, 8 bits)))
    val bank = if (spec.mapper == 3) RegNextWhen(prgSlave.cmd.data(1 downto 0), prgSlave.writeFire).init(0) else U(0, 2 bits)
    /** The bank once this cycle's access has written it, for the PPU's dot after the access. */
    val bankAfter = if (spec.mapper == 3) prgSlave.writeFire ? prgSlave.cmd.data(1 downto 0) | bank else bank
    def chrIndex(a: UInt, after: Boolean = false) = ((if (after) bankAfter else bank) @@ a(12 downto 0)).resize(log2Up(spec.chrBytes))
  }

  /** A read port on CHR at a PPU address (0000-1FFF). Call it from a build phase. With
    * `afterAccess`, a bank switch written by this cycle's CPU access already applies. */
  def chrRead(address: UInt, afterAccess: Boolean = false): UInt = logic.chr.readAsync(logic.chrIndex(address, afterAccess))
  /** The PPU's write port, used only with CHR RAM. */
  def chrWrite(address: UInt, data: UInt, enable: Bool): Unit = if (spec.chrRam) logic.chr.write(logic.chrIndex(address), data, enable)
  /** A nametable address (2000-3FFF) to an index into the console's 2 KB of VRAM. */
  def ciramIndex(address: UInt): UInt =
    if (spec.verticalMirroring) address(10 downto 0) else (address(11) ## address(9 downto 0)).asUInt

  /** Copies an iNES image's PRG and CHR into the simulated cartridge. */
  def load(c: NesCartridge): Unit = {
    for (i <- c.prg.indices) logic.prg.setBigInt(i, c.prg(i))
    if (!spec.chrRam) for (i <- c.chr.indices) logic.chr.setBigInt(i, c.chr(i))
  }
}
