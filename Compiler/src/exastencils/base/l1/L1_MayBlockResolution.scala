package exastencils.base.l1

import exastencils.core.StateManager

/// L1_MayBlockResolution

object L1_MayBlockResolution {
  // no wrapping since provided statement/ expression is usually subclass of L1_MayBlockResolution
  def isDone(stmt : L1_Statement) = StateManager.findFirst({ mayBlock : L1_MayBlockResolution => !mayBlock.allDone }, stmt).isEmpty
  def isDone(expr : L1_Expression) = StateManager.findFirst({ mayBlock : L1_MayBlockResolution => !mayBlock.allDone }, expr).isEmpty
}

trait L1_MayBlockResolution {
  var allDone = false
}
