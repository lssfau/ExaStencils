package exastencils.operator.l2

import exastencils.knowledge.l2._
import exastencils.operator.l3._

/// L2_StencilCollection

object L2_StencilCollection extends L2_LeveledKnowledgeCollection[L2_Stencil, L3_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_UnfoldLeveledDeclarations.strategies += L2_UnfoldStencilDeclarations
  L2_PrepareDeclarations.strategies += L2_PrepareStencilDeclaration
  L2_PrepareAccesses.strategies += L2_PrepareStencilAccesses
  L2_ProcessDeclarations.strategies += L2_ProcessStencilDeclarations
  L2_ResolveAccesses.strategies += L2_ResolveStencilAccesses

  def progress() = objects.foreach(obj => L3_StencilCollection.add(obj.progress()))
}
