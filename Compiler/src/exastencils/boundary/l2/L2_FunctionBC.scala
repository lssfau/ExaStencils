package exastencils.boundary.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_FunctionCall
import exastencils.boundary.l3.L3_FunctionBC
import exastencils.prettyprinting.PpStream

/// L2_FunctionBC

// wraps a user-provided function implementing boundary handling
case class L2_FunctionBC(boundaryFunction : L2_FunctionCall) extends L2_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryFunction
  override def progress = ProgressLocation(L3_FunctionBC(boundaryFunction.progress))
}
