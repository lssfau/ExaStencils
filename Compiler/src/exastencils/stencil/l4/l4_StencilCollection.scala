package exastencils.stencil.l4

import exastencils.knowledge.StencilCollection
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_StencilCollection extends L4_LeveledKnowledgeCollection[L4_Stencil] {
  def progress = {
    for (obj <- objects)
      StencilCollection.stencils += obj.progress
  }
}
