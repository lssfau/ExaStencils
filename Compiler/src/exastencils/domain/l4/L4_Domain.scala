package exastencils.domain.l4

import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_Domain

abstract class L4_Domain extends L4_KnowledgeObject[IR_Domain] {
  def numDims : Int

//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = IR_Domain(name)
}

/// L4_DummyDomain

case class L4_DummyDomain() extends L4_Domain {
  override def name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l4 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l4 dummy domain; unsupported")
}
