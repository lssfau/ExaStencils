package exastencils.boundary.l4

import exastencils.base.l4._
import exastencils.boundary.ir.IR_FunctionBC
import exastencils.prettyprinting.PpStream

/// L4_FunctionBC

// wraps a user-provided function implementing boundary handling
case class L4_FunctionBC(var boundaryFunction : L4_FunctionCall) extends L4_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryFunction
  override def progress = IR_FunctionBC(boundaryFunction.progress)
}
