package exastencils.knowledge.ir

/// IR_KnowledgeObject

trait IR_KnowledgeObject {
  def name : String
}

/// IR_KnowledgeObjectWithLevel

trait IR_KnowledgeObjectWithLevel extends IR_KnowledgeObject {
  def level : Int
}
