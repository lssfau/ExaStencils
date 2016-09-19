package exastencils.interfacing.l4

import exastencils.knowledge._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection

object L4_ExternalFieldCollection extends L4_LeveledKnowledgeCollection[L4_ExternalField] {
  def progress = {
    for (obj <- objects)
      ExternalFieldCollection.fields += obj.progress
  }
}
