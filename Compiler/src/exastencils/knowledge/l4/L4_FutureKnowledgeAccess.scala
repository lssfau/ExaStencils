package exastencils.knowledge.l4

import exastencils.base.l4._
import exastencils.core.StateManager

/// L4_FutureKnowledgeAccess

object L4_FutureKnowledgeAccess {
  def existsInStmt(stmt : L4_Statement) = StateManager.findFirst[L4_FutureKnowledgeAccess](stmt).isDefined
}

trait L4_FutureKnowledgeAccess extends L4_Access
