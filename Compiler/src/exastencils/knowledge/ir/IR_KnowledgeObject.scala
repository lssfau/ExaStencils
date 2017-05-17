package exastencils.knowledge.ir

/// IR_KnowledgeObject

trait IR_KnowledgeObject {

  exastencils.core.Duplicate.dontClone(this.getClass)

  def name : String
}

/// IR_LeveledKnowledgeObject

trait IR_LeveledKnowledgeObject extends IR_KnowledgeObject {
  def level : Int
}
