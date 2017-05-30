package exastencils.operator.l3

import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.operator.l4._

/// L3_StencilFieldCollection

object L3_StencilFieldCollection extends L3_LeveledKnowledgeCollection[L3_StencilField, L4_StencilField] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareStencilFieldDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessStencilFieldDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareStencilFieldAccesses
  L3_ResolveAccesses.strategies += L3_ResolveStencilFieldAccesses

  override def name = "L3_StencilFieldCollection"
  override def progress() = objects.foreach(obj => L4_StencilFieldCollection.add(obj.progress()))
}
