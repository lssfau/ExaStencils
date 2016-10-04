package exastencils.stencil.l4

import exastencils.knowledge._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_StencilFieldCollection extends L4_LeveledKnowledgeCollection[L4_StencilField] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress = {
    for (obj <- objects)
      StencilFieldCollection.stencilFields += obj.progress
  }
}


