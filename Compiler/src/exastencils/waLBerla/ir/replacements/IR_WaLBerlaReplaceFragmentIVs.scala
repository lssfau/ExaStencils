package exastencils.waLBerla.ir.replacements

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.grid.IR_WaLBerlaBlockAABB

object IR_WaLBerlaReplaceFragmentIVs extends IR_WaLBerlaReplacementStrategy("Replace frag info accesses with accesses to waLBerla block info") {
  def block = IR_WaLBerlaLoopOverLocalBlocks.block

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
