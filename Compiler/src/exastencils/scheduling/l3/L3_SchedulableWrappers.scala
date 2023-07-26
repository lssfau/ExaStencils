package exastencils.scheduling.l3

import exastencils.config.Knowledge
import exastencils.field.l3.L3_FieldCollection
import exastencils.grid.l3._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.scheduling.NoStrategyWrapper
import exastencils.solver.l3.L3_ProcessSolverForEquations


/// L3_ProcessDeclarationsAndResolveAccessesWrapper

object L3_ProcessDeclarationsAndResolveAccessesWrapper extends NoStrategyWrapper {

  def callback : () => Unit = () => {
    var matches = 0
    do {
      matches = 0
      matches += L3_ProcessDeclarations.applyAndCountMatches()

      L3_ProcessSolverForEquations.apply()
      matches += (if (L3_ProcessSolverForEquations.results.isEmpty) 0 else L3_ProcessSolverForEquations.results.last._2.matches)

      matches += L3_ResolveAccesses.applyAndCountMatches()

      if (Knowledge.experimental_l3_resolveVirtualFields) {
        // integrate before evaluate -> might be nested
        L3_ResolveIntegrateOnGrid.apply()
        matches += (if (L3_ResolveIntegrateOnGrid.results.isEmpty) 0 else L3_ResolveIntegrateOnGrid.results.last._2.matches)

        L3_ResolveEvaluateOnGrid.apply()
        matches += (if (L3_ResolveEvaluateOnGrid.results.isEmpty) 0 else L3_ResolveEvaluateOnGrid.results.last._2.matches)
      }
    } while (matches > 0)
  }

}

/// L3_AddInitFieldsFunctionWrapper

object L3_AddInitFieldsFunctionWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => L3_FieldCollection.addInitFieldsFunction()
}

