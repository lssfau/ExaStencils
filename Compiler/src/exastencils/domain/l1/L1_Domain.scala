package exastencils.domain.l1

import exastencils.config.Knowledge
import exastencils.domain.l2.L2_Domain
import exastencils.knowledge.l1.L1_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_Domain

abstract class L1_Domain extends L1_KnowledgeObject[L2_Domain] {
  def numDims : Int

//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = L2_Domain(name)
}

/// L1_DummyDomain

case class L1_DummyDomain() extends L1_Domain {
  override def name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l1 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l1 dummy domain; unsupported")
}
