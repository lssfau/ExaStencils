package exastencils.base.l2

import exastencils.base.l3.L3_Node

/// L2_Progressable

trait L2_Progressable {
  def progress : L3_Node
}

/// L2_ProgressOption

object L2_ProgressOption {
  def apply[L2_Type <: L2_Progressable, L3_Type <: L3_Node](toProgress : Option[L2_Type])(progFct : (L2_Type => L3_Type)) : Option[L3_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
