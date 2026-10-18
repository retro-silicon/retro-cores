package retro.sm83

import org.scalatest.funsuite.AnyFunSuite
import retro.core._
import retro.sim._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Cosimulation: the RTL and `Sm83Reference` run the same memory image and see the same
  * interrupt requests; every retired instruction must match in M-cycle count, ordered bus
  * accesses and architectural registers. */
class Sm83CosimSim extends AnyFunSuite {
  lazy val compiled = SimConfig.withVerilator.compile(Sm83Core())
  val implemented = Sm83.names.keys.filter(_ < 0x100).toIndexedSeq.sorted

  /** Checks each RTL retire against the reference as it happens. */
  class CosimBackend(ref: Sm83Reference) extends TraceBackend {
    var checked = 0
    val seen = collection.mutable.Map[Int, Int]().withDefaultValue(0)
    override def onRetire(e: RetireEvent) = {
      val name = Sm83.names.getOrElse(e.opcode, "?")
      val (refBus, refCycles) = ref.step()
      val ctx = f"instruction #${e.index} at ${e.address}%04X ${e.opcode}%03X $name"
      assert(e.bus == refBus, s"$ctx: bus\n  rtl ${e.bus.mkString(", ")}\n  ref ${refBus.mkString(", ")}")
      assert(e.mcycles == refCycles, s"$ctx: ${e.mcycles} M-cycles, reference says $refCycles")
      val rtlRegs = Sm83.architectural.map(n => n -> e.regs(n)).toMap
      assert(rtlRegs == ref.regs, s"$ctx: registers\n  rtl $rtlRegs\n  ref ${ref.regs}")
      seen(e.opcode) += 1
      checked += 1
    }
  }

  case class Result(seen: Map[Int, Int], mem: Array[Int], regs: Map[String, BigInt])

  /** @param irqRate chance per M-cycle that a random IF bit is raised (IE is all ones) */
  def cosim(image: Array[Int], instructions: Int, seed: Int, waitStates: Boolean, irqRate: Double = 0,
            irqAt: Map[Long, Int] = Map.empty): Result = {
    var result: Result = null
    compiled.doSimUntilVoid(seed = seed) { dut =>
      val io = dut.host[BusPlugin].logic.io
      val int = dut.host[Sm83.InterruptPlugin].logic
      val done = dut.host[TracePlugin].logic.done
      val mem = image.clone()
      // IE & IF as the RTL saw it in each completed M-cycle, for the reference.
      val requestLog = ArrayBuffer[Int]()
      val ref = new Sm83Reference(image.clone(), k => requestLog(k.toInt))
      val cosim = new CosimBackend(ref)
      var refReset = false

      val rnd = new Random(seed)
      var flagsIF = 0
      io.stall #= false
      int.requests #= 0
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.onSamplings {
        if (done.toBoolean) {
          requestLog += int.requests.toInt
          if (!refReset) { ref.reset(); refReset = true }
        }
        if (io.cmd.valid.toBoolean && io.cmd.write.toBoolean && !io.stall.toBoolean) mem(io.cmd.address.toInt) = io.cmd.data.toInt
        if (int.ack.valid.toBoolean) flagsIF &= ~(1 << int.ack.payload.toInt)
        if (rnd.nextDouble() < irqRate) flagsIF |= 1 << rnd.nextInt(5)
        for (bits <- irqAt.get(requestLog.size)) flagsIF |= bits
        int.requests #= flagsIF
        io.stall #= waitStates && rnd.nextInt(4) == 0
      }
      new Probe(dut.host[TracePlugin], dut.clockDomain).add(cosim)
      fork { while (true) { io.rdata #= mem(io.cmd.address.toInt); sleep(1) } }

      // A HALT with nothing to wake it would wait forever; stop on a cycle budget as well.
      var cycles = 0L
      dut.clockDomain.onSamplings(cycles += 1)
      dut.clockDomain.waitSamplingWhere(cosim.checked >= instructions || cycles > instructions * 50L)
      result = Result(cosim.seen.toMap, mem, ref.regs)
      simSuccess()
    }
    result
  }

  def image(bytes: (Int, Seq[Int])*) = {
    val m = Array.fill(0x10000)(0)
    for ((base, bs) <- bytes; (v, i) <- bs.zipWithIndex) m(base + i) = v
    m
  }

  test("hand-written program: copy loop with 16-bit pointers and flags") {
    // LD HL,C000 / LD DE,D000 / LD B,05 / loop: LD A,(HL+) / ADD A,B / LD (DE),A / INC DE / DEC B / JR NZ,loop
    // then: XOR A / CP 01 / JR C,+2 / ... / JP 0000
    val prog = Seq(0x21, 0x00, 0xC0, 0x11, 0x00, 0xD0, 0x06, 0x05,
      0x2A, 0x80, 0x12, 0x13, 0x05, 0x20, 0xF9,
      0xAF, 0xFE, 0x01, 0x38, 0x02, 0x3E, 0x99, 0xC3, 0x00, 0x00)
    cosim(image(0 -> prog, 0xC000 -> (0 until 5).map(i => 0x10 * i + 0x0F)), 200, seed = 1, waitStates = false)
  }

  test("calls, stack and CB ops compute a known result") {
    // LD SP,FFFE / LD A,81 / CALL 0100 / LD (C000),A / PUSH AF / POP BC / HALT-free loop: JR -2
    // 0100: RLC A / SWAP A / SET 1,A / BIT 7,A / RET
    val prog = Seq(0x31, 0xFE, 0xFF, 0x3E, 0x81, 0xCD, 0x00, 0x01, 0xEA, 0x00, 0xC0, 0xF5, 0xC1, 0x18, 0xFE)
    val sub = Seq(0xCB, 0x07, 0xCB, 0x37, 0xCB, 0xCF, 0xCB, 0x7F, 0xC9)
    val r = cosim(image(0 -> prog, 0x100 -> sub), 20, seed = 2, waitStates = true)
    // RLC 81 = 03 (C=1), SWAP = 30 (C=0, Z=0), SET 1 = 32, BIT 7 -> Z=1, H=1
    assert(r.mem(0xC000) == 0x32)
    assert(r.regs("B") == 0x32 && r.regs("C") == 0xA0)
  }

  test("EI, HALT and an interrupt run the handler and return") {
    // LD SP,FFFE / EI / HALT / LD (C000),A / JR -2 ; handler at 0x48 (STAT, IF bit 1): LD A,5A / RETI
    val prog = Seq(0x31, 0xFE, 0xFF, 0xFB, 0x76, 0xEA, 0x00, 0xC0, 0x18, 0xFE)
    val r = cosim(image(0 -> prog, 0x48 -> Seq(0x3E, 0x5A, 0xD9)), 10, seed = 3, waitStates = false, irqAt = Map(40L -> 0x02))
    assert(r.mem(0xC000) == 0x5A)
    assert(r.seen(Sm83.DISPATCH) == 1 && r.seen(0x76) == 1)
  }

  // Memory filled entirely with implemented opcodes: every byte, operand or not, decodes, so
  // random jumps, calls and pointer writes always land on code. Random code soon falls into a
  // loop (RST to itself, JR -2...), so coverage comes from many short runs rather than one long one.
  for ((name, wait, irq) <- Seq(("rare interrupts", false, 0.002), ("frequent interrupts", false, 0.02), ("wait states and interrupts", true, 0.02))) {
    test(s"random code, $name") {
      val seen = collection.mutable.Map[Int, Int]().withDefaultValue(0)
      for (seed <- 0 until 40) {
        val rnd = new Random(seed * 7919 + name.length)
        // One byte in eight is 0xCB, so the CB page gets exercised (its second byte is any value).
        val mem = Array.fill(0x10000)(if (rnd.nextInt(8) == 0) 0xCB else implemented(rnd.nextInt(implemented.size)))
        val r = cosim(mem, 1500, seed, wait, irq)
        for ((o, n) <- r.seen) seen(o) += n
      }
      val base = seen.keys.count(_ < Sm83.CB_PAGE)
      val cb = seen.keys.count(o => o >= Sm83.CB_PAGE && o < Sm83.DISPATCH)
      println(s"random code, $name: ${seen.values.sum} instructions, $base/${implemented.size} base opcodes, $cb/256 CB, ${seen(Sm83.DISPATCH)} interrupts")
      assert(base >= implemented.size * 9 / 10, s"only $base base opcodes exercised")
      assert(cb > 200, s"only $cb CB opcodes exercised")
      assert(seen(Sm83.DISPATCH) > 20)
    }
  }
}
