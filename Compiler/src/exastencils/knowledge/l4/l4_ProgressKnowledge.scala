package exastencils.knowledge.l4

import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.stencil.l4._

object L4_ProgressKnowledge {
  def apply() = {
    L4_FieldLayoutCollection.progress
    L4_FieldCollection.progress
    L4_ExternalFieldCollection.progress
    L4_StencilCollection.progress
    L4_StencilFieldCollection.progress
  }
}