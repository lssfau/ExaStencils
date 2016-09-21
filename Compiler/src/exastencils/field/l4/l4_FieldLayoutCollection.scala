package exastencils.field.l4

import exastencils.knowledge._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_FieldLayoutCollection extends L4_LeveledKnowledgeCollection[L4_FieldLayout] {
  def progress = {
    for (obj <- objects)
      FieldLayoutCollection.fieldLayouts += obj.progress
  }
}