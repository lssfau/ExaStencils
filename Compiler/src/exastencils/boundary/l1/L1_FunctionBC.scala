package exastencils.boundary.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1.L1_FunctionCall
import exastencils.boundary.l2.L2_FunctionBC
import exastencils.prettyprinting.PpStream

/// L1_FunctionBC

// wraps a user-provided function implementing boundary handling
case class L1_FunctionBC(boundaryFunction : L1_FunctionCall) extends L1_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryFunction
  override def progress = ProgressLocation(L2_FunctionBC(boundaryFunction.progress))
}
