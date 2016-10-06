package exastencils.domain.ir

import exastencils.config._
import exastencils.domain._
import exastencils.knowledge.FileInputDomain
import exastencils.knowledge.ir.IR_KnowledgeCollection

object IR_DomainCollection extends IR_KnowledgeCollection[IR_Domain] {
  def initFragments() = {
    val global = getByIdentifier("global").get

    if (Knowledge.domain_readFromFile)
      global.shape.asInstanceOf[List[FileInputDomain]].foreach { f => f.shape.initFragments() }
    else if (Knowledge.domain_useCase != "")
      global.shape.asInstanceOf[ShapedDomainShape].initFragments()
    else
      global.shape.asInstanceOf[DomainShape].initFragments()
  }
}
