package exastencils.knowledge.l4

import exastencils.domain.l4.L4_HACK_ProcessDomainDeclarations
import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ProcessExternalFieldDeclarations
import exastencils.stencil.l4._

object L4_ProcessKnowledgeDeclarations {
  def apply() = {
    L4_HACK_ProcessDomainDeclarations.apply()
    L4_ProcessStencilDeclarations.apply()
    L4_ProcessFieldLayoutDeclarations.apply()
    L4_ProcessFieldDeclarations.apply()
    L4_ProcessExternalFieldDeclarations.apply()
    L4_ProcessStencilFieldDeclarations.apply()
  }
}
