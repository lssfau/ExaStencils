package exastencils.knowledge.ir

/// IR_KnowledgeObject

trait IR_KnowledgeObject {

  exastencils.core.Duplicate.dontClone(this.getClass)

  def name : String
}

/// IR_KnowledgeObjectWithLevel

trait IR_KnowledgeObjectWithLevel extends IR_KnowledgeObject {
  def level : Int
}
