package exastencils.scheduling.l1

import exastencils.domain.l1.L1_DomainCollection
import exastencils.knowledge.l1.L1_KnowledgeContainer.L1_ProcessDeclarations
import exastencils.knowledge.l1.L1_KnowledgeContainer.L1_ResolveAccesses
import exastencils.scheduling.NoStrategyWrapper

/// L1_ProcessDeclarationsAndResolveAccessesWrapper

object L1_ProcessDeclarationsAndResolveAccessesWrapper extends NoStrategyWrapper {

  def callback : () => Unit = () => {
    var matches = 0
    do {
      matches = 0
      matches += L1_ProcessDeclarations.applyAndCountMatches()
      matches += L1_ResolveAccesses.applyAndCountMatches()

    } while (matches > 0)
  }
}

/// L1_HandleDomainAliasesWrapper

object L1_HandleDomainAliasesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => L1_DomainCollection.handleAliases()
}
