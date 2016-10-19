package exastencils.stencil.l4

import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.stencil.ir._

object L4_StencilFieldCollection extends L4_LeveledKnowledgeCollection[L4_StencilField, IR_StencilField] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = {
    for (obj <- objects)
      IR_StencilFieldCollection.objects += obj.progress
  }
}


