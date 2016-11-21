package exastencils.knowledge.l4

import exastencils.domain.l4.L4_DomainCollection
import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.operator.l4.L4_StencilCollection
import exastencils.stencil.l4._

object L4_ClearKnowledge {
  def apply() = {
    L4_DomainCollection.clear()
    L4_FieldLayoutCollection.clear()
    L4_FieldCollection.clear()
    L4_ExternalFieldCollection.clear()
    L4_StencilCollection.clear()
    L4_StencilFieldCollection.clear()
  }
}
