package exastencils.solver.l1

import exastencils.knowledge.l1.L1_KnowledgeContainer._
import exastencils.knowledge.l1._
import exastencils.solver.l2._

/// L1_EquationCollection

object L1_EquationCollection extends L1_LeveledKnowledgeCollection[L1_NamedEquation, L2_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)

  L1_KnowledgeContainer.register(this)

  L1_PrepareDeclarations.strategies += L1_PrepareEquationDeclarations
  L1_ProcessDeclarations.strategies += L1_ProcessEquationDeclarations

  L1_PrepareAccesses.strategies += L1_PrepareEquationAccesses
  L1_ResolveAccesses.strategies += L1_ResolveEquationAccesses

  override def name = "L1_EquationCollection"
  override def progress() = objects.foreach(obj => L2_EquationCollection.add(obj.progress()))
}
