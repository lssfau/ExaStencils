package exastencils.field.l4

import exastencils.field.ir.IR_FieldCollection
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_FieldCollection extends L4_LeveledKnowledgeCollection[L4_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress = {
    for (obj <- objects)
      IR_FieldCollection.objects += obj.progress
  }
}
