package exastencils.knowledge.l3

import exastencils.base.l3.L3_Access

/// L3_KnowledgeAccess

trait L3_KnowledgeAccess extends L3_Access {
  def target : L3_KnowledgeObject[_]
  override def name : String = target.name
}

/// L3_LeveledKnowledgeAccess

trait L3_LeveledKnowledgeAccess extends L3_KnowledgeAccess {
  override def target : L3_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
