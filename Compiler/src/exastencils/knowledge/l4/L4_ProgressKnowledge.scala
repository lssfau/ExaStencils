package exastencils.knowledge.l4

import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.operator.l4._

object L4_ProgressKnowledge {
  def apply() = {
    // may require:
    //L4_DomainCollection.progress()

    // may require:
    L4_FieldLayoutCollection.progress()

    // may require: Domain, FieldLayout
    L4_FieldCollection.progress()

    // may require: FieldLayout, Field
    L4_ExternalFieldCollection.progress()

    // may require: Field for non-linear stencils
    L4_StencilCollection.progress()

    // may require: Stencil(Template) and Field
    L4_StencilFieldCollection.progress()
  }
}
