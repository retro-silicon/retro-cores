package retro.gb

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.sim._
import retro.sm83._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** System cosimulation: the `GameBoy` RTL (SM83 and SoC plugins in one host) against
  * `GbReference`. Every retired instruction must match in M-cycles, ordered bus accesses (so
  * every I/O register read, timer and LCD state included) and CPU registers, and the serial
  * port must send the same bytes. */
class GameBoySim extends AnyFunSuite {
  lazy val compiled = SimConfig.withVerilator.compile(GameBoy())
  val implemented = Sm83.names.keys.filter(_ < 0x100).toIndexedSeq.sorted

  case class Result(seen: Map[Int, Int], serial: Seq[Int], ref: GbReference)

  /** @param cart        the cartridge: answers every address the SoC leaves external
    * @param buttonRate  chance per M-cycle that the joypad inputs change
    * @param maxCycles   stop here even if fewer instructions retired (HALT can wait forever) */
  def cosim(cart: Array[Int], instructions: Int, seed: Int, buttonRate: Double = 0, maxCycles: Long = 0): Result = {
    var result: Result = null
    compiled.doSimUntilVoid(seed = seed) { dut =>
      val io = dut.host[BusPlugin].logic.io
      val buttons = dut.host[JoypadPlugin].buttons
      val tx = dut.host[SerialPlugin].tx
      val done = dut.host[TracePlugin].logic.done
      val mem = cart.clone()
      val buttonLog = ArrayBuffer[Int]()
      val ref = new GbReference(cart.clone())
      val cpu = ref.cpu
      var refReset = false
      val serial = ArrayBuffer[Int]()
      val seen = collection.mutable.Map[Int, Int]().withDefaultValue(0)
      var checked = 0

      val cosim = new TraceBackend {
        override def onRetire(e: RetireEvent) = {
          val name = Sm83.names.getOrElse(e.opcode, "?")
          val (refBus, refCycles) = cpu.step()
          val ctx = f"instruction #${e.index} at ${e.address}%04X ${e.opcode}%03X $name"
          assert(e.bus == refBus, s"$ctx: bus\n  rtl ${e.bus.mkString(", ")}\n  ref ${refBus.mkString(", ")}")
          assert(e.mcycles == refCycles, s"$ctx: ${e.mcycles} M-cycles, reference says $refCycles")
          val rtlRegs = Sm83Regs.architectural.map(r => r.name -> e.regs(r.name)).toMap
          assert(rtlRegs == cpu.regs, s"$ctx: registers\n  rtl $rtlRegs\n  ref ${cpu.regs}")
          seen(e.opcode) += 1
          // A halted M-cycle retires as HALTED; it does not count towards the instruction budget.
          if (e.opcode != Sm83.HALTED) checked += 1
        }
      }
      ref.buttonsAt = k => buttonLog(k.toInt)

      val rnd = new Random(seed)
      var pressed = 0
      io.stall #= false
      buttons #= 0
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings {
        if (done.toBoolean) {
          buttonLog += pressed
          if (!refReset) { cpu.reset(); refReset = true }
        }
        if (io.cmd.valid.toBoolean && io.cmd.write.toBoolean && io.cmd.address.toInt >= 0x8000) mem(io.cmd.address.toInt) = io.cmd.data.toInt
        if (tx.valid.toBoolean) serial += tx.payload.toInt
        if (rnd.nextDouble() < buttonRate) { pressed = rnd.nextInt(256); buttons #= pressed }
      }
      new Probe(dut.host[TracePlugin], dut.clockDomain).add(cosim)
      fork { while (true) { io.rdata #= mem(io.cmd.address.toInt); sleep(1) } }

      var cycles = 0L
      dut.clockDomain.onSamplings(cycles += 1)
      val budget = if (maxCycles > 0) maxCycles else instructions * 50L
      dut.clockDomain.waitSamplingWhere(checked >= instructions || cycles > budget)
      assert(serial == ref.serialOut, s"serial: rtl $serial, reference ${ref.serialOut}")
      result = Result(seen.toMap, serial.toList, ref)
      simSuccess()
    }
    result
  }

  def cart(bytes: (Int, Seq[Int])*) = {
    val m = Array.fill(0x10000)(0)
    for ((base, bs) <- bytes; (v, i) <- bs.zipWithIndex) m(base + i) = v
    m
  }

  test("timer interrupts and serial output") {
    val main = Seq(
      0xF3, //             DI
      0x31, 0xFE, 0xFF, // LD SP,FFFE
      0x3E, 0x04, //       LD A,04
      0xE0, 0xFF, //       LDH (IE),A      timer interrupt only
      0xAF, //             XOR A
      0xE0, 0x0F, //       LDH (IF),A
      0xE0, 0x80, //       LDH (FF80),A    interrupt count
      0x3E, 0xF0, //       LD A,F0
      0xE0, 0x06, //       LDH (TMA),A
      0xE0, 0x05, //       LDH (TIMA),A
      0x3E, 0x05, //       LD A,05
      0xE0, 0x07, //       LDH (TAC),A     on, one tick per 4 M-cycles
      0xFB, //             EI
      0x76, //       0118: HALT
      0xF0, 0x80, //       LDH A,(FF80)
      0xFE, 0x05, //       CP 05
      0x20, 0xF9, //       JR NZ,0118
      0x3E, 0x4F, //       LD A,'O'
      0xCD, 0x30, 0x01, // CALL 0130
      0x3E, 0x4B, //       LD A,'K'
      0xCD, 0x30, 0x01, // CALL 0130
      0x18, 0xFE) //       JR $
    val send = Seq(
      0xE0, 0x01, //       0130: LDH (SB),A
      0x3E, 0x81, //       LD A,81
      0xE0, 0x02, //       LDH (SC),A      start, internal clock
      0xF0, 0x02, //       0136: LDH A,(SC)
      0xCB, 0x7F, //       BIT 7,A
      0x20, 0xFA, //       JR NZ,0136
      0xC9) //             RET
    val handler = Seq(0xF5, 0xF0, 0x80, 0x3C, 0xE0, 0x80, 0xF1, 0xD9) // PUSH AF / count++ / POP AF / RETI
    val r = cosim(cart(0x100 -> main, 0x130 -> send, 0x50 -> handler), 1500, seed = 1)
    assert(r.serial.map(_.toChar).mkString == "OK")
    assert(r.ref.hram(0) >= 5, s"only ${r.ref.hram(0)} timer interrupts")
    assert(r.seen(Sm83.DISPATCH) >= 5)
  }

  test("LCD timing: VBlank interrupt and LY") {
    // Wait for VBlank by interrupt, then read LY twice; the handler stores LY at FF81.
    val main = Seq(
      0x31, 0xFE, 0xFF, // LD SP,FFFE
      0x3E, 0x01, //       LD A,01
      0xE0, 0xFF, //       LDH (IE),A      VBlank only
      0xAF, //             XOR A
      0xE0, 0x0F, //       LDH (IF),A
      0xFB, //             EI
      0x76, //       010B: HALT
      0x18, 0xFD) //       JR 010B
    val handler = Seq(0xF0, 0x44, 0xE0, 0x81, 0xD9) // LDH A,(LY) / LDH (FF81),A / RETI
    val r = cosim(cart(0x100 -> main, 0x40 -> handler), 30, seed = 2, maxCycles = 40000)
    assert(r.seen(Sm83.DISPATCH) >= 2)
    assert(r.ref.hram(1) == 144, s"LY in the VBlank handler: ${r.ref.hram(1)}")
  }

  // Random code from the cartridge, after a prologue that enables every interrupt source, a fast
  // timer and all STAT sources. Random writes reach every I/O register, the joypad inputs change
  // at random, and every read of them must match the model.
  for ((name, buttonRate) <- Seq(("random code", 0.0), ("random code with joypad input", 0.01))) {
    test(name) {
      val seen = collection.mutable.Map[Int, Int]().withDefaultValue(0)
      val prologue = Seq(0x3E, 0x1F, 0xE0, 0xFF, 0x3E, 0x05, 0xE0, 0x07, 0x3E, 0x78, 0xE0, 0x41, 0xFB)
      for (seed <- 0 until 30) {
        val rnd = new Random(seed * 104729 + name.length)
        val c = Array.fill(0x10000)(if (rnd.nextInt(8) == 0) 0xCB else implemented(rnd.nextInt(implemented.size)))
        for (a <- 0x8000 until 0xA000) c(a) = 0
        for ((v, i) <- prologue.zipWithIndex) c(0x100 + i) = v
        val r = cosim(c, 1500, seed, buttonRate)
        for ((o, n) <- r.seen) seen(o) += n
      }
      val base = seen.keys.count(implemented.contains)
      println(s"$name: ${seen.values.sum} instructions, $base/${implemented.size} base opcodes, ${seen(Sm83.DISPATCH)} interrupts")
      assert(base >= implemented.size * 9 / 10, s"only $base base opcodes exercised")
      assert(seen(Sm83.DISPATCH) > 20)
    }
  }
}
