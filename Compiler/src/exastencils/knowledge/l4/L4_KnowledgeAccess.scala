package exastencils.knowledge.l4

import exastencils.base.l4.L4_Access

/// L4_KnowledgeAccess

trait L4_KnowledgeAccess extends L4_Access {
  def target : L4_KnowledgeObject[_]
  override def name : String = target.name
}

/// L4_LeveledKnowledgeAccess

trait L4_LeveledKnowledgeAccess extends L4_KnowledgeAccess {
  override def target : L4_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
