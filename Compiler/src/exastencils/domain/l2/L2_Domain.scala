package exastencils.domain.l2

import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_Domain

case class L2_Domain(var name : String /* TODO: add other relevant information */) extends L2_KnowledgeObject {
  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progress : L3_KnowledgeObject = ???
  override def getProgressedObject : L3_KnowledgeObject = ???
}
