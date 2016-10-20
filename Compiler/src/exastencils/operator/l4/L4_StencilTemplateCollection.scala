package exastencils.operator.l4

import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.operator.ir._

object L4_StencilTemplateCollection extends L4_LeveledKnowledgeCollection[L4_StencilTemplate, IR_StencilTemplate] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = {
    for (obj <- objects)
      IR_StencilTemplateCollection.add(obj.progress)
  }
}

