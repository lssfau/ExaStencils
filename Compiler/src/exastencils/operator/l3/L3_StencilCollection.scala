package exastencils.operator.l3

import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.operator.l4._

/// L3_StencilCollection

object L3_StencilCollection extends L3_LeveledKnowledgeCollection[L3_Stencil, L4_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareStencilDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessStencilDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareStencilAccesses
  L3_ResolveAccesses.strategies += L3_ResolveStencilAccesses

  override def name = "L3_StencilCollection"
  override def progress() = objects.foreach(obj => L4_StencilCollection.add(obj.progress()))
}
