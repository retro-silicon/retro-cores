package retro.sim

import spinal.core._
import spinal.core.internals._
import spinal.core.sim._

/** Verilator settings for long system simulations.
  *
  * SpinalSim marks every `simPublic` signal and every memory `verilator public`, which tells
  * Verilator the testbench may write them between evaluations, so it re-evaluates all logic that
  * depends on them on every `eval()`, both clock edges included. On a whole system (register
  * file, memories, bus taps) that is nearly all of it, twice per clock. `public_flat_rd` keeps
  * them readable and lets Verilator evaluate that logic only after the clock edge. The testbench
  * may still write memories (images loaded before the first clock), but must not force
  * registers or wires. */
object FastVerilator {
  object publicRead extends AttributeFlag("verilator public_flat_rd", COMMENT_ATTRIBUTE)

  /** Swaps the marker while the Verilog is emitted, then restores it: SpinalSim checks for it
    * when the testbench accesses a signal. */
  def install(config: SpinalConfig): Unit = config.phasesInserters += { phases =>
    val swapped = scala.collection.mutable.ArrayBuffer[SpinalTagReady]()
    val i = phases.indexWhere(_.isInstanceOf[SimVerilatorPhase])
    require(i >= 0, "SimVerilatorPhase not found")
    phases.insert(
      i + 1,
      new PhaseNetlist {
        override def impl(pc: PhaseContext): Unit = {
          pc.walkDeclarations {
            case d: SpinalTagReady if d.hasTag(Verilator.public) =>
              d.removeTag(Verilator.public); d.addTag(publicRead); swapped += d
            case _ =>
          }
          // The C++ wrapper reaches signals through the scope Verilator generates only when one
          // signal is `public`: a constant nothing reads.
          pc.topLevel.rework { val scope = Bool().setName("verilator_scope"); scope := False; scope.addTag(Verilator.public) }
        }
      }
    )
    phases += new PhaseNetlist {
      override def impl(pc: PhaseContext): Unit = for (d <- swapped) { d.removeTag(publicRead); d.addTag(Verilator.public) }
    }
  }

  /** A Verilator `SimConfig` with C++ at -O3 and read-only public signals. */
  def apply(config: SpinalConfig = SpinalConfig()): SpinalSimConfig = {
    install(config)
    SimConfig.withVerilator.withConfig(config).allOptimisation.addSimulatorFlag("-O3")
  }
}
