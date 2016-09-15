package exastencils.base.l2

import exastencils.base.l3.L3_Node

trait L2_Progressable {
  def progress : L3_Node
}

object L2_ProgressOption {
  def apply[L2Type <: L2_Progressable, L3Type <: L3_Node](toProgress : Option[L2Type])(progFct : (L2Type => L3Type)) : Option[L3Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
