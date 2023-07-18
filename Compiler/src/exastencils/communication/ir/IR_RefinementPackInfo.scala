package exastencils.communication.ir

import exastencils.communication.RefinementCases
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.logger.Logger

/// IR_RefinementPackInfo

trait IR_RefinementPackInfo extends IR_PackInfo {
  field.localization match {
    case IR_AtCellCenter =>
    case _               => Logger.error("Mesh refinement is currently only available for cell-centered discretizations.")
  }

  def refinementCase : RefinementCases.Access

  def indexOfRefinedNeighbor : Int
}

/// IR_F2CPackInfo

trait IR_F2CPackInfo extends IR_RefinementPackInfo {
  override def refinementCase = RefinementCases.F2C
}

/// IR_C2FPackInfo

trait IR_C2FPackInfo extends IR_RefinementPackInfo {
  override def refinementCase = RefinementCases.C2F
}
