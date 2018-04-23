package exastencils.domain.l1

import exastencils.config.Knowledge
import exastencils.domain.l2.L2_Domain
import exastencils.knowledge.l1.L1_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_Domain

abstract class L1_Domain extends L1_KnowledgeObject[L2_Domain] {
  var name : String
  def numDims : Int

  override def createDuplicate() : L1_KnowledgeObject[L2_Domain] = Logger.error("Trying to duplicate an l1 domain. This is currently unsupported.")
}

/// L1_DummyDomain

case class L1_DummyDomain() extends L1_Domain {
  override var name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l1 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l1 dummy domain; unsupported")
}
