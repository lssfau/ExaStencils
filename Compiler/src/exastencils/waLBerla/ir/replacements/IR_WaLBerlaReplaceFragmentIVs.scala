package exastencils.waLBerla.ir.replacements

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Negation
import exastencils.communication.DefaultNeighbors
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.grid.IR_WaLBerlaBlockAABB
import exastencils.waLBerla.ir.util.IR_WaLBerlaDirection
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

object IR_WaLBerlaReplaceFragmentIVs extends IR_WaLBerlaReplacementStrategy("Replace frag info accesses with accesses to waLBerla block info") {
  def block = IR_WaLBerlaLoopOverBlocks.block

  def getBlockAABB = IR_WaLBerlaBlockAABB(block)

  this += Transformation("Replace", {

    /* TODO
    case iv : IR_IV_FragmentId if inWaLBerlaScope(collector)       => ...
    case iv : IR_IV_FragmentIndex if inWaLBerlaScope(collector)    => ...
    case iv : IR_IV_IsValidForDomain if inWaLBerlaScope(collector) => ...
    */

    // TODO: fragment connection

    /* assignments */
    case assign @ IR_Assignment(fragPos, _, "=") if inWaLBerlaBlockLoop(collector) =>
      fragPos match {
        case _ @ IR_IV_FragmentPosition(dim, _)      => assign.src = getBlockAABB.center(dim)
        case _ @ IR_IV_FragmentPositionBegin(dim, _) => assign.src = getBlockAABB.min(dim)
        case _ @ IR_IV_FragmentPositionEnd(dim, _)   => assign.src = getBlockAABB.max(dim)
        case _                                       =>
      }
      assign

    /* accesses */
    case _ @ IR_IV_FragmentPosition(dim, _) if inWaLBerlaBlockLoop(collector) => getBlockAABB.center(dim)
    case _ @ IR_IV_FragmentPositionBegin(dim, _) if inWaLBerlaBlockLoop(collector) => getBlockAABB.min(dim)
    case _ @ IR_IV_FragmentPositionEnd(dim, _) if inWaLBerlaBlockLoop(collector)  => getBlockAABB.max(dim)

    /* conditions */

  }, recursive = false)
}
