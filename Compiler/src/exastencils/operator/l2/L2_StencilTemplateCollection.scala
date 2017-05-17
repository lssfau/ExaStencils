package exastencils.operator.l2

import exastencils.knowledge.l2._
import exastencils.operator.l3._

/// L2_StencilTemplateCollection

object L2_StencilTemplateCollection extends L2_LeveledKnowledgeCollection[L2_StencilTemplate, L3_StencilTemplate] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_UnfoldLeveledDeclarations.strategies += L2_UnfoldStencilTemplateDeclarations
  L2_PrepareDeclarations.strategies += L2_PrepareStencilTemplateDeclarations
//  L2_PrepareAccesses.strategies += L2_PrepareStencilTemplateAccesses
  L2_ProcessDeclarations.strategies += L2_ProcessStencilTemplateDeclarations
//  L2_ResolveAccesses.strategies += L2_ResolveStencilTemplateAccesses

  def progress() = objects.foreach(obj => L3_StencilTemplateCollection.add(obj.progress()))
}
