package exastencils.stencil.ir

import exastencils.field.ir.IR_Field
import exastencils.knowledge.ir._

/// IR_StencilField

object IR_StencilField {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class IR_StencilField(
    var identifier : String,
    var level : Int,
    var field : IR_Field,
    var stencil : IR_Stencil) extends IR_KnowledgeObjectWithIdentAndLevel {
}
