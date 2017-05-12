package exastencils.knowledge.ir

import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir._
import exastencils.interfacing.ir.IR_ExternalFieldCollection
import exastencils.operator.ir._

object IR_ClearKnowledge {
  def apply() = {
    IR_DomainCollection.clear()
    IR_FieldLayoutCollection.clear()
    IR_FieldCollection.clear()
    IR_ExternalFieldCollection.clear()
    IR_StencilCollection.clear()
    IR_StencilFieldCollection.clear()
  }
}
