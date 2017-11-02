package exastencils.domain.l1

import exastencils.domain.l2._
import exastencils.knowledge.l1.L1_KnowledgeContainer._
import exastencils.knowledge.l1._

/// L1_DomainCollection

object L1_DomainCollection extends L1_BasicKnowledgeCollection[L1_Domain, L2_Domain] {
  exastencils.core.Duplicate.registerConstant(this)

  L1_KnowledgeContainer.register(this)

  L1_PrepareDeclarations.strategies += L1_PrepareDomainDeclarations
  L1_ProcessDeclarations.strategies += L1_ProcessDomainDeclarations

  L1_PrepareAccesses.strategies += L1_PrepareDomainAccesses
  L1_ResolveAccesses.strategies += L1_ResolveDomainAccesses

  override def name = "L1_DomainCollection"
  override def progress() = objects.foreach(obj => L2_DomainCollection.add(obj.progress()))
}
