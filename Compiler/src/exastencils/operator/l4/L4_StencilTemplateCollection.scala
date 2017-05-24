package exastencils.operator.l4

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.knowledge.l4._

/// L4_StencilTemplateCollection

object L4_StencilTemplateCollection extends L4_LeveledKnowledgeCollection[L4_StencilTemplate, IR_KnowledgeObject] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

//  L4_PrepareDeclarations.strategies += L4_PrepareStencilTemplateDeclarations
//  L4_ProcessDeclarations.strategies += L4_ProcessStencilTemplateDeclarations

//  L4_PrepareAccesses.strategies += L4_PrepareStencilTemplateAccesses
//  L4_ResolveAccesses.strategies += L4_ResolveStencilTemplateAccesses

  override def name = "L4_StencilTemplateCollection"
  override def progress() = {} // FIXME: objects.foreach(obj => IR_StencilTemplateCollection.add(obj.progress()))
}
