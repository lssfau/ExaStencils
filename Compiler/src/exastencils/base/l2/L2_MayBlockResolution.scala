package exastencils.base.l2

import exastencils.core.StateManager

/// L2_MayBlockResolution

object L2_MayBlockResolution {
  // no wrapping since provided statement/ expression is usually subclass of L2_MayBlockResolution
  def isDone(stmt : L2_Statement) = StateManager.findFirst({ mayBlock : L2_MayBlockResolution => !mayBlock.allDone }, stmt).isEmpty
  def isDone(expr : L2_Expression) = StateManager.findFirst({ mayBlock : L2_MayBlockResolution => !mayBlock.allDone }, expr).isEmpty
}

trait L2_MayBlockResolution {
  var allDone = false
}
