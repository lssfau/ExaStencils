package exastencils.field.l4

import exastencils.knowledge._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_FieldCollection extends L4_LeveledKnowledgeCollection[L4_Field] {
  def progress = {
    for (obj <- objects)
      FieldCollection.fields += obj.progress
  }
}
