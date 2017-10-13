package exastencils.operator.l4

import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.operator.ir._

/// L4_StencilCollection

object L4_StencilCollection extends L4_LeveledKnowledgeCollection[L4_Stencil, IR_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareStencilDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessStencilDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareStencilAccesses
  L4_ResolveAccesses.strategies += L4_ResolveStencilAccesses

  override def name = "L4_StencilCollection"
  override def progress() = objects.foreach(obj => IR_StencilCollection.add(obj.progress()))
}
