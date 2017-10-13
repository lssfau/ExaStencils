package exastencils.solver.l4

import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.solver.ir._

/// L4_EquationCollection

object L4_EquationCollection extends L4_LeveledKnowledgeCollection[L4_NamedEquation, IR_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareEquationDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessEquationDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareEquationAccesses
  L4_ResolveAccesses.strategies += L4_ResolveEquationAccesses

  override def name = "L4_EquationCollection"
  override def progress() = objects.foreach(obj => IR_EquationCollection.add(obj.progress()))
}
