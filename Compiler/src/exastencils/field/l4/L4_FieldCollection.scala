package exastencils.field.l4

import exastencils.field.ir._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_FieldCollection extends L4_LeveledKnowledgeCollection[L4_Field, IR_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = {
    for (obj <- objects)
      IR_FieldCollection.objects += obj.progress
  }
}
