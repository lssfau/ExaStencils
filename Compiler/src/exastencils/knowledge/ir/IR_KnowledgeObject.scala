package exastencils.knowledge.ir

trait IR_KnowledgeObject {}

trait IR_KnowledgeObjectWithIdent extends IR_KnowledgeObject {
  def identifier : String
}

trait IR_KnowledgeObjectWithLevel extends IR_KnowledgeObjectWithIdent {
  def level : Int
}

trait IR_KnowledgeObjectWithIdentAndLevel extends IR_KnowledgeObjectWithIdent with IR_KnowledgeObjectWithLevel
