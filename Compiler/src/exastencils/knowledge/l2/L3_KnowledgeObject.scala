package exastencils.knowledge.l2

import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_KnowledgeObject

trait L2_KnowledgeObject {
  def name : String
  def progress : L3_KnowledgeObject
  def prettyprintDecl(out : PpStream) : Unit
  def getProgressedObject : L3_KnowledgeObject
}

/// L2_KnowledgeObjectWithLevel

trait L2_KnowledgeObjectWithLevel extends L2_KnowledgeObject {
  def level : Int
}
