package exastencils.domain.ir

import exastencils.config._
import exastencils.deprecated.domain._
import exastencils.knowledge.ir.IR_KnowledgeCollection

/// IR_DomainCollection

object IR_DomainCollection extends IR_KnowledgeCollection[IR_Domain] {
  def initFragments() = {
    val global = getByIdentifier("global").get

    if (Knowledge.domain_readFromFile)
      global.HACK_shape.asInstanceOf[List[FileInputDomain]].foreach { f => f.HACK_shape.initFragments() }
    else if (Knowledge.domain_useCase != "")
      global.HACK_shape.asInstanceOf[ShapedDomainShape].initFragments()
    else
      global.HACK_shape.asInstanceOf[DomainShape].initFragments()
  }
}
