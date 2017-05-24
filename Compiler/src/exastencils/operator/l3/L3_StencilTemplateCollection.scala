package exastencils.operator.l3

import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.operator.l4._

/// L3_StencilTemplateCollection

object L3_StencilTemplateCollection extends L3_LeveledKnowledgeCollection[L3_StencilTemplate, L4_StencilTemplate] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareStencilTemplateDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessStencilTemplateDeclarations

//  L3_PrepareAccesses.strategies += L3_PrepareStencilTemplateAccesses
//  L3_ResolveAccesses.strategies += L3_ResolveStencilTemplateAccesses

  override def name = "L3_StencilTemplateCollection"
  override def progress() = objects.foreach(obj => L4_StencilTemplateCollection.add(obj.progress()))
}
