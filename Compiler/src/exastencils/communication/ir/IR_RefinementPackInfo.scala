package exastencils.communication.ir

import exastencils.communication.RefinementCases

/// IR_RefinementPackInfo

trait IR_RefinementPackInfo extends IR_PackInfo {
  def refinementCase : RefinementCases.Access

  def refinementNeighborIndex : Int
}

/// IR_F2CPackInfo

trait IR_F2CPackInfo extends IR_RefinementPackInfo {
  override def refinementCase = RefinementCases.F2C
}

/// IR_C2FPackInfo

trait IR_C2FPackInfo extends IR_RefinementPackInfo {
  override def refinementCase = RefinementCases.C2F
}
