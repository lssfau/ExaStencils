package exastencils.domain.ir

import exastencils.knowledge.ir.IR_KnowledgeObject

/// IR_Domain

object IR_Domain {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

trait IR_Domain extends IR_KnowledgeObject {
  def index : Int
  def shape : Any
}
