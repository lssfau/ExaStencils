package exastencils.base.l4

import exastencils.core.StateManager

/// L4_MayBlockResolution

object L4_MayBlockResolution {
  // no wrapping since provided statement/ expression is usually subclass of L4_MayBlockResolution
  def isDone(stmt : L4_Statement) = StateManager.findFirst({ mayBlock : L4_MayBlockResolution => !mayBlock.allDone }, stmt).isEmpty
  def isDone(expr : L4_Expression) = StateManager.findFirst({ mayBlock : L4_MayBlockResolution => !mayBlock.allDone }, expr).isEmpty
}

trait L4_MayBlockResolution {
  var allDone = false
}
