package exastencils.domain.l4

import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L4_Domain

// FIXME: merge with the deprecated domain system
case class L4_Domain(var name : String /* TODO: add other relevant information */) extends L4_KnowledgeObject[IR_Domain] {
  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = ???
}
