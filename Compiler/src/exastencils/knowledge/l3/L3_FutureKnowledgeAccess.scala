package exastencils.knowledge.l3

import exastencils.base.l3._
import exastencils.core.StateManager

/// L3_FutureKnowledgeAccess

object L3_FutureKnowledgeAccess {
  def existsInStmt(stmt : L3_Statement) = StateManager.findFirst[L3_FutureKnowledgeAccess](stmt).isDefined
}

trait L3_FutureKnowledgeAccess extends L3_Access
