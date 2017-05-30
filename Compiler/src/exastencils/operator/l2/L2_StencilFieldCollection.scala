package exastencils.operator.l2

import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._
import exastencils.operator.l3._

/// L2_StencilFieldCollection

object L2_StencilFieldCollection extends L2_LeveledKnowledgeCollection[L2_StencilField, L3_StencilField] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_KnowledgeContainer.register(this)

  L2_PrepareDeclarations.strategies += L2_PrepareStencilFieldDeclarations
  L2_ProcessDeclarations.strategies += L2_ProcessStencilFieldDeclarations

  L2_PrepareAccesses.strategies += L2_PrepareStencilFieldAccesses
  L2_ResolveAccesses.strategies += L2_ResolveStencilFieldAccesses

  override def name = "L2_StencilFieldCollection"
  override def progress() = objects.foreach(obj => L3_StencilFieldCollection.add(obj.progress()))
}
