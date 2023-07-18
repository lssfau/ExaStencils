package exastencils.communication.ir

import exastencils.communication.RefinementCases
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.logger.Logger

/// IR_RefinementPackInfo

trait IR_RefinementPackInfo extends IR_PackInfo {

  /* constraints (aka TODOs) */

  // cell-centered only
  field.localization match {
    case IR_AtCellCenter =>
    case _               => Logger.error("Mesh refinement is currently only available for cell-centered discretizations.")
  }

  // total layout size per dim is divisible by two
  if ((0 until numDimsGrid).exists(dim => field.layout.defTotal(dim) % 2 != 0))
   Logger.error(s"Total layout size of ${field.codeName} per dim must be divisible by two when using mesh refinement.")

  // no comm of duplicate layers
  if (field.layout.communicatesDuplicated)
    Logger.error("Communication of duplicate layers in refined meshes is not implemented.")

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
