package exastencils.boundary.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_FunctionCall
import exastencils.boundary.l4.L4_FunctionBC
import exastencils.prettyprinting.PpStream

/// L3_FunctionBC

// wraps a user-provided function implementing boundary handling
case class L3_FunctionBC(boundaryFunction : L3_FunctionCall) extends L3_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryFunction
  override def progress = ProgressLocation(L4_FunctionBC(boundaryFunction.progress))
}
