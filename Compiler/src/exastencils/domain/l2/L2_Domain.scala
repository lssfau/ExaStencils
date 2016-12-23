package exastencils.domain.l2

import exastencils.domain.l3.L3_Domain
import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_Domain

case class L2_Domain(var name : String /* TODO: add other relevant information */) extends L2_KnowledgeObject[L3_Domain] {
  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L3_Domain(name)
}
