package exastencils.base.l3

import exastencils.core.StateManager

/// L3_MayBlockResolution

object L3_MayBlockResolution {
  // no wrapping since provided statement/ expression is usually subclass of L3_MayBlockResolution
  def isDone(stmt : L3_Statement) = StateManager.findFirst({ mayBlock : L3_MayBlockResolution => !mayBlock.allDone }, stmt).isEmpty
  def isDone(expr : L3_Expression) = StateManager.findFirst({ mayBlock : L3_MayBlockResolution => !mayBlock.allDone }, expr).isEmpty
}

trait L3_MayBlockResolution {
  var allDone = false
}
