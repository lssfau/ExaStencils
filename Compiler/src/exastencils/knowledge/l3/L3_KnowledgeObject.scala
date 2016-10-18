package exastencils.knowledge.l3

import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L3_KnowledgeObject

trait L3_KnowledgeObject {
  def name : String
  def progress : L4_KnowledgeObject
  def prettyprintDecl(out : PpStream) : Unit
  def getProgressedObject : L4_KnowledgeObject
}

/// L3_KnowledgeObjectWithLevel

trait L3_KnowledgeObjectWithLevel extends L3_KnowledgeObject {
  def level : Int
}
