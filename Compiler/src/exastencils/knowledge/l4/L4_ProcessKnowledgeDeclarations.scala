package exastencils.knowledge.l4

import exastencils.domain.l4.L4_HACK_ProcessDomainDeclarations
import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ProcessExternalFieldDeclarations
import exastencils.stencil.l4._

object L4_ProcessKnowledgeDeclarations {
  def apply() = {
    // may require:
    L4_HACK_ProcessDomainDeclarations.apply()

    // may require:
    L4_ProcessStencilDeclarations.apply()

    // may require:
    L4_ProcessFieldLayoutDeclarations.apply()

    // may require: Domain, FieldLayout
    L4_ProcessFieldDeclarations.apply()

    // may require: Field
    L4_ProcessExternalFieldDeclarations.apply()

    // may require: Stencil(Template) and Field
    L4_ProcessStencilFieldDeclarations.apply()
  }
}
