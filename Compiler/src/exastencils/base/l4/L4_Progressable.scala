package exastencils.base.l4

import exastencils.base.ir.IR_Node

/// L4_Progressable

trait L4_Progressable {
  def progress : IR_Node
}

/// L4_ProgressOption

object L4_ProgressOption {
  def apply[L4Type <: L4_Progressable, IRType <: IR_Node](toProgress : Option[L4Type])(progFct : (L4Type => IRType)) : Option[IRType] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
