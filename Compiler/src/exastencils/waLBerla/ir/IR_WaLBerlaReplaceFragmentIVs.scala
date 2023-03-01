package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.grid.IR_WaLBerlaBlockAABB

object IR_WaLBerlaReplaceFragmentIVs extends DefaultStrategy("Replace frag info accesses with accesses to waLBerla block info") {

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def block = IR_WaLBerlaLoopOverBlocks.block

  def inWaLBerlaBlockLoop(collector : IR_StackCollector) =
    collector.stack.exists {
      case _ : IR_WaLBerlaLoopOverBlocks              => true
      case _                                          => false
    }


  def getBlockAABB = IR_WaLBerlaBlockAABB(block)

  this += Transformation("Replace", {

    /* TODO
    case iv : IR_IV_FragmentId if inWaLBerlaScope(collector)       => ...
    case iv : IR_IV_FragmentIndex if inWaLBerlaScope(collector)    => ...
    case iv : IR_IV_IsValidForDomain if inWaLBerlaScope(collector) => ...
    */

    // TODO: fragment connection
    case assign @ IR_Assignment(fragPos, _, "=") if inWaLBerlaBlockLoop(collector) =>
      fragPos match {
        case _ @ IR_IV_FragmentPosition(dim, _)      => assign.src = getBlockAABB.center(dim)
        case _ @ IR_IV_FragmentPositionBegin(dim, _) => assign.src = getBlockAABB.min(dim)
        case _ @ IR_IV_FragmentPositionEnd(dim, _)   => assign.src = getBlockAABB.max(dim)
        case _                                       =>
      }
      assign

    case _ @ IR_IV_FragmentPosition(dim, _) if inWaLBerlaBlockLoop(collector) => getBlockAABB.center(dim)

    case _ @ IR_IV_FragmentPositionBegin(dim, _) if inWaLBerlaBlockLoop(collector) => getBlockAABB.min(dim)

    case _ @ IR_IV_FragmentPositionEnd(dim, _) if inWaLBerlaBlockLoop(collector)  => getBlockAABB.max(dim)
  }, recursive = false)
}
