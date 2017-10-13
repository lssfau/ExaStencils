package exastencils.knowledge.ir

import exastencils.base.ir.IR_Access

/// IR_KnowledgeAccess

trait IR_KnowledgeAccess extends IR_Access {
  def target : IR_KnowledgeObject
  /*override*/ def name : String = target.name
}

/// IR_LeveledKnowledgeAccess

trait IR_LeveledKnowledgeAccess extends IR_KnowledgeAccess {
  override def target : IR_LeveledKnowledgeObject
  override def name : String = target.name
  def level : Int = target.level
}
