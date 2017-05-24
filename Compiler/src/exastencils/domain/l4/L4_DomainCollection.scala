package exastencils.domain.l4

import exastencils.domain.ir._
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._

/// L4_DomainCollection

object L4_DomainCollection extends L4_BasicKnowledgeCollection[L4_Domain, IR_Domain] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareDomainDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessDomainDeclarations

  override def name = "L4_DomainCollection"
  override def progress() = objects.foreach(obj => IR_DomainCollection.add(obj.progress()))
}
