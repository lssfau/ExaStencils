package exastencils.interfacing.l4

import exastencils.interfacing.ir._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_ExternalFieldCollection extends L4_LeveledKnowledgeCollection[L4_ExternalField, IR_ExternalField] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = {
    for (obj <- objects)
      IR_ExternalFieldCollection.objects += obj.progress
  }
}
