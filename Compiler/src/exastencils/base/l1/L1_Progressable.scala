package exastencils.base.l1

import exastencils.base.l2.L2_Node

/// L1_Progressable

trait L1_Progressable {
  def progress : L2_Node
}

/// L1_ProgressOption

object L1_ProgressOption {
  def apply[L1_Type <: L1_Progressable, L2_Type <: L2_Node](toProgress : Option[L1_Type])(progFct : (L1_Type => L2_Type)) : Option[L2_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
