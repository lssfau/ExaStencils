package exastencils.solver.l2

import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._
import exastencils.solver.l3._

/// L2_EquationCollection

object L2_EquationCollection extends L2_LeveledKnowledgeCollection[L2_NamedEquation, L3_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_KnowledgeContainer.register(this)

  L2_PrepareDeclarations.strategies += L2_PrepareEquationDeclarations
  L2_ProcessDeclarations.strategies += L2_ProcessEquationDeclarations

  L2_PrepareAccesses.strategies += L2_PrepareEquationAccesses
  L2_ResolveAccesses.strategies += L2_ResolveEquationAccesses

  override def name = "L2_EquationCollection"
  override def progress() = objects.foreach(obj => L3_EquationCollection.add(obj.progress()))
}
