package exastencils.operator.l3

import exastencils.knowledge.l3.L3_LeveledKnowledgeCollection
import exastencils.operator.l4._

/// L3_StencilCollection

object L3_StencilCollection extends L3_LeveledKnowledgeCollection[L3_Stencil, L4_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = {
    for (obj <- objects)
      L4_StencilCollection.add(obj.progress())
  }
}
