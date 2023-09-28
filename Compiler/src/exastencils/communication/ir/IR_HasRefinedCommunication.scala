package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.RefinementCase

trait IR_HasRefinedCommunication {

  def getIndexOfRefinedNeighbor(packInfo : IR_PackInfo) : Option[IR_Expression] = packInfo match {
    case p : IR_RefinementPackInfo => Some(p.indexOfRefinedNeighbor)
    case _                         => None
  }
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
