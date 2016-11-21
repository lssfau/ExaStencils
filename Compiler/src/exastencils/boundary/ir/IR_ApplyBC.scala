package exastencils.boundary.ir

import exastencils.base.ir._
import exastencils.deprecated.ir.IR_FieldSelection

/// IR_ApplyBC

case class IR_ApplyBC(var field : IR_FieldSelection) extends IR_Statement with IR_SpecialExpandable {}
