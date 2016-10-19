package exastencils.field.l2

import exastencils.field.l3.L3_Field
import exastencils.knowledge.l2._
import exastencils.knowledge.l3.L3_FieldCollection

/// L2_FieldCollection

object L2_FieldCollection extends L2_LeveledKnowledgeCollection[L2_Field, L3_Field] {
  def progress() = {
    for (obj <- objects)
      L3_FieldCollection.add(obj.progress())
  }
}
