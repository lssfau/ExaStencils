package exastencils.domain.l3

import exastencils.domain.l4.L4_Domain
import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L3_Domain

case class L3_Domain(var name : String /* TODO: add other relevant information */) extends L3_KnowledgeObject[L4_Domain] {
  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = ???
}
