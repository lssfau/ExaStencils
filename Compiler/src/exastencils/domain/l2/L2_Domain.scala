package exastencils.domain.l2

import exastencils.config.Knowledge
import exastencils.domain.l3.L3_Domain
import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_Domain

abstract class L2_Domain extends L2_KnowledgeObject[L3_Domain] {
  def numDims : Int

//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = L3_Domain(name)
}

/// L2_DummyDomain

case class L2_DummyDomain() extends L2_Domain {
  override def name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l2 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l2 dummy domain; unsupported")
}
