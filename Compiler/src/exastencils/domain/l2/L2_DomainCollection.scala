package exastencils.domain.l2

import exastencils.domain.l3._
import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._

/// L2_DomainCollection

object L2_DomainCollection extends L2_BasicKnowledgeCollection[L2_Domain, L3_Domain] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_KnowledgeContainer.register(this)

  L2_PrepareDeclarations.strategies += L2_PrepareDomainDeclarations
  L2_ProcessDeclarations.strategies += L2_ProcessDomainDeclarations

  override def name = "L2_DomainCollection"
  override def progress() = objects.foreach(obj => L3_DomainCollection.add(obj.progress()))
}
