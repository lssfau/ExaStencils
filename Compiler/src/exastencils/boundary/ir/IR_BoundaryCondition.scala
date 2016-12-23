package exastencils.boundary.ir

import exastencils.base.ir.IR_Node

/// IR_BoundaryCondition

trait IR_BoundaryCondition extends IR_Node

/// IR_NoBC

case object IR_NoBC extends IR_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)
}
