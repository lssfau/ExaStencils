package exastencils.boundary.ir

import exastencils.base.ir._
import exastencils.field.ir.IR_Field

/// IR_ApplyBC

case class IR_ApplyBC(var field : IR_Field, var slot : IR_Expression) extends IR_Statement with IR_SpecialExpandable {}
