package exastencils.operator.l1

import exastencils.knowledge.l1.L1_KnowledgeContainer._
import exastencils.knowledge.l1._
import exastencils.operator.l2._

/// L1_OperatorCollection

object L1_OperatorCollection extends L1_LeveledKnowledgeCollection[L1_Operator, L2_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  L1_KnowledgeContainer.register(this)

  L1_PrepareDeclarations.strategies += L1_PrepareOperatorDeclarations
  L1_ProcessDeclarations.strategies += L1_ProcessOperatorDeclarations

  L1_PrepareAccesses.strategies += L1_PrepareOperatorAccesses
  L1_ResolveAccesses.strategies += L1_ResolveOperatorAccesses

  override def name = "L1_OperatorCollection"
  override def progress() = objects.foreach(obj => L2_StencilCollection.add(obj.progress()))
}
