package exastencils.field.l4

import exastencils.field.ir._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_FieldLayoutCollection extends L4_LeveledKnowledgeCollection[L4_FieldLayout, IR_FieldLayout] {
  def progress() = {
    for (obj <- objects)
      IR_FieldLayoutCollection.objects += obj.progress
  }
}