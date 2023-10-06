package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_RefinementIndexForCoarseNeighbor
import exastencils.domain.ir.RefinementCase

trait IR_HasRefinedCommunication {

  def getIndexOfRefinedNeighbor(packInfo : IR_PackInfo) : Option[IR_Expression] = packInfo match {
    case p : IR_RefinementPackInfo => Some(p.indexOfRefinedNeighbor)
    case _                         => None
  }

  def isCurrentFineNeighbor(refinementCase : RefinementCase.Access, domainIdx : IR_Expression, neighbor : NeighborInfo, indexOfRefinedNeighbor : Option[IR_Expression]) =
    if (refinementCase == RefinementCase.F2C && indexOfRefinedNeighbor.isDefined)
      IR_EqEq(indexOfRefinedNeighbor.get, IR_RefinementIndexForCoarseNeighbor(neighbor.index, domainIdx))
    else
      IR_BooleanConstant(true)
}

trait IR_HasRefinedPacking extends IR_HasRefinedCommunication {
  def equalLevelCopyLoop() : IR_Statement
  def coarseToFineCopyLoop() : IR_Statement
  def fineToCoarseCopyLoop() : IR_Statement

  def getCopyLoop(refinementCase: RefinementCase.Access) : IR_Statement = {
    if (Knowledge.refinement_enabled) {
      refinementCase match {
        case RefinementCase.EQUAL =>
          equalLevelCopyLoop()
        case RefinementCase.C2F   =>
          coarseToFineCopyLoop()
        case RefinementCase.F2C   =>
          fineToCoarseCopyLoop()
      }
    } else {
      equalLevelCopyLoop()
    }
  }
}
