package exastencils.knowledge.l4

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.prettyprinting.PpStream

trait L4_KnowledgeObject {
  def prettyprintDecl(out : PpStream) : Unit
  def progress : IR_KnowledgeObject
  def getProgressedObject : IR_KnowledgeObject
}

trait L4_KnowledgeObjectWithIdent extends L4_KnowledgeObject {
  def identifier : String
}

trait L4_KnowledgeObjectWithLevel extends L4_KnowledgeObjectWithIdent {
  def level : Int
}

trait L4_KnowledgeObjectWithIdentAndLevel extends L4_KnowledgeObjectWithIdent with L4_KnowledgeObjectWithLevel
