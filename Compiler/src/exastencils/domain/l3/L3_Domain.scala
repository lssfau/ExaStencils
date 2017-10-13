package exastencils.domain.l3

import exastencils.config.Knowledge
import exastencils.domain.l4.L4_Domain
import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_Domain

abstract class L3_Domain extends L3_KnowledgeObject[L4_Domain] {
  def numDims : Int

//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = L4_Domain(name)
}

/// L3_DummyDomain

case class L3_DummyDomain() extends L3_Domain {
  override def name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l3 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l3 dummy domain; unsupported")
}
