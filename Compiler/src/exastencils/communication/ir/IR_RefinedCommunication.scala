package exastencils.communication.ir

import exastencils.base.ir.IR_Statement
import exastencils.config.Knowledge
import exastencils.domain.ir.RefinementCase

trait IR_RefinedCommunication {
  def refinementCase : RefinementCase.Access

  def equalLevelCopyLoop() : IR_Statement
  def coarseToFineCopyLoop() : IR_Statement
  def fineToCoarseCopyLoop() : IR_Statement

  def getCopyLoop() : IR_Statement = {
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
