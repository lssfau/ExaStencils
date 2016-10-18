package exastencils.knowledge.l4

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L4_KnowledgeObject

trait L4_KnowledgeObject {
  def name : String
  def progress : IR_KnowledgeObject
  def prettyprintDecl(out : PpStream) : Unit
  def getProgressedObject : IR_KnowledgeObject
}

/// L4_KnowledgeObjectWithLevel

trait L4_KnowledgeObjectWithLevel extends L4_KnowledgeObject {
  def level : Int
}
