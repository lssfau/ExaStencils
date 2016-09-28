package exastencils.domain.ir

import exastencils.knowledge.ir.IR_KnowledgeObjectWithIdent

trait IR_Domain extends IR_KnowledgeObjectWithIdent {
  def identifier : String
  def index : Int
  def shape : Any
}
