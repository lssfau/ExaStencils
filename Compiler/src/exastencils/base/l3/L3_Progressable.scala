package exastencils.base.l3

import exastencils.base.l4.L4_Node

/// L3_Progressable

trait L3_Progressable {
  def progress : L4_Node
}

/// L3_ProgressOption

object L3_ProgressOption {
  def apply[L3_Type <: L3_Progressable, L4_Type <: L4_Node](toProgress : Option[L3_Type])(progFct : (L3_Type => L4_Type)) : Option[L4_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
