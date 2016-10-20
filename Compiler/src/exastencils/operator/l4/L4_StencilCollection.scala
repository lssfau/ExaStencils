package exastencils.operator.l4

import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.operator.ir._

/// L4_StencilCollection

object L4_StencilCollection extends L4_LeveledKnowledgeCollection[L4_Stencil, IR_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = {
    for (obj <- objects)
      IR_StencilCollection.add(obj.progress())
  }
}
