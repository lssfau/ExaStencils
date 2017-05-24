package exastencils.domain.l3

import exastencils.domain.l4._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._

/// L3_DomainCollection

object L3_DomainCollection extends L3_BasicKnowledgeCollection[L3_Domain, L4_Domain] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareDomainDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessDomainDeclarations

  override def name = "L3_DomainCollection"
  override def progress() = objects.foreach(obj => L4_DomainCollection.add(obj.progress()))
}
