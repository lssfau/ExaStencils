package exastencils.operator.l4

import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.operator.ir._

/// L4_StencilFieldCollection

object L4_StencilFieldCollection extends L4_LeveledKnowledgeCollection[L4_StencilField, IR_StencilField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareStencilFieldDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessStencilFieldDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareStencilFieldAccesses
  L4_ResolveAccesses.strategies += L4_ResolveStencilFieldAccesses

  override def name = "L4_StencilFieldCollection"
  override def progress() = objects.foreach(obj => IR_StencilFieldCollection.add(obj.progress()))
}
