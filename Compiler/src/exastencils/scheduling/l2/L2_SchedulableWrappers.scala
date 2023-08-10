package exastencils.scheduling.l2

import exastencils.config.Knowledge
import exastencils.grid.l2._
import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.scheduling.NoStrategyWrapper

/// L2_ProcessDeclarationsAndResolveAccessesWrapper

object L2_ProcessDeclarationsAndResolveAccessesWrapper extends NoStrategyWrapper {

  def callback : () => Unit = () => {
    var matches = 0
    do {
      matches = 0
      matches += L2_ProcessDeclarations.applyAndCountMatches()
      matches += L2_ResolveAccesses.applyAndCountMatches()

      if (Knowledge.experimental_l2_resolveVirtualFields) {
        // integrate before evaluate -> might be nested
        L2_ResolveIntegrateOnGrid.apply()
        matches += (if (L2_ResolveIntegrateOnGrid.results.isEmpty) 0 else L2_ResolveIntegrateOnGrid.results.last._2.matches)

        L2_ResolveEvaluateOnGrid.apply()
        matches += (if (L2_ResolveEvaluateOnGrid.results.isEmpty) 0 else L2_ResolveEvaluateOnGrid.results.last._2.matches)
      }
    } while (matches > 0)
  }
}