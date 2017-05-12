package exastencils.base.l3

import exastencils.base.l4.L4_Node

trait L3_Progressable {
  def progress : L4_Node
}

object L3_ProgressOption {
  def apply[L3Type <: L3_Progressable, L4Type <: L4_Node](toProgress : Option[L3Type])(progFct : (L3Type => L4Type)) : Option[L4Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
