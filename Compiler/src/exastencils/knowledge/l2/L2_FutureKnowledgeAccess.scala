package exastencils.knowledge.l2

import exastencils.base.l2._
import exastencils.core.StateManager

/// L2_FutureKnowledgeAccess

object L2_FutureKnowledgeAccess {
  def existsInStmt(stmt : L2_Statement) = StateManager.findFirst[L2_FutureKnowledgeAccess](stmt).isDefined
}

trait L2_FutureKnowledgeAccess extends L2_Access
