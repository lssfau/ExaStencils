package exastencils.knowledge.l1

import exastencils.base.l1.L1_Access

/// L1_KnowledgeAccess

trait L1_KnowledgeAccess extends L1_Access {
  def target : L1_KnowledgeObject[_]
  override def name : String = target.name
}

/// L1_LeveledKnowledgeAccess

trait L1_LeveledKnowledgeAccess extends L1_KnowledgeAccess {
  override def target : L1_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
