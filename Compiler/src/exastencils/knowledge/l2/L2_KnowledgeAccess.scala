package exastencils.knowledge.l2

import exastencils.base.l2.L2_Access

/// L2_KnowledgeAccess

trait L2_KnowledgeAccess extends L2_Access {
  def target : L2_KnowledgeObject[_]
  override def name : String = target.name
}

/// L2_LeveledKnowledgeAccess

trait L2_LeveledKnowledgeAccess extends L2_KnowledgeAccess {
  override def target : L2_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
