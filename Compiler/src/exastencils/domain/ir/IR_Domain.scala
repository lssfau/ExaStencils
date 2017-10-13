package exastencils.domain.ir

import exastencils.knowledge.ir.IR_KnowledgeObject

/// IR_Domain

object IR_Domain {
  var runningIndex = 0
}

trait IR_Domain extends IR_KnowledgeObject {
  def numDims : Int
  def index : Int
  def HACK_shape : Any
}
