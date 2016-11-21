package exastencils.operator.l2

import exastencils.knowledge.l2._
import exastencils.operator.l3._

/// L2_StencilCollection

object L2_StencilCollection extends L2_LeveledKnowledgeCollection[L2_Stencil, L3_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = {
    for (obj <- objects)
      L3_StencilCollection.add(obj.progress())
  }
}
