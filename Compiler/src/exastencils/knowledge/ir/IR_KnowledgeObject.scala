package exastencils.knowledge.ir

/// IR_KnowledgeObject

object IR_KnowledgeObject {
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[IR_KnowledgeObject])
}

trait IR_KnowledgeObject {

  IR_KnowledgeObject

  def name : String
  def createDuplicate() : IR_KnowledgeObject = ???
}

/// IR_LeveledKnowledgeObject

trait IR_LeveledKnowledgeObject extends IR_KnowledgeObject {
  def level : Int
}
