package exastencils.base.l4

import exastencils.base.ir.IR_Node

/// L4_Progressable

trait L4_Progressable {
  def progress : IR_Node
}

/// L4_ProgressOption

object L4_ProgressOption {
  def apply[L4_Type <: L4_Progressable, IR_Type <: IR_Node](toProgress : Option[L4_Type])(progFct : (L4_Type => IR_Type)) : Option[IR_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
