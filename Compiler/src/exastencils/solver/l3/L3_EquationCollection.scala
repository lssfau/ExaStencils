package exastencils.solver.l3

import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.solver.l4._

/// L3_EquationCollection

object L3_EquationCollection extends L3_LeveledKnowledgeCollection[L3_NamedEquation, L4_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareEquationDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessEquationDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareEquationAccesses
  L3_ResolveAccesses.strategies += L3_ResolveEquationAccesses

  override def name = "L3_EquationCollection"
  override def progress() = objects.foreach(obj => L4_EquationCollection.add(obj.progress()))
}
