package exastencils.boundary.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.boundary.l3._
import exastencils.prettyprinting._

/// L2_BoundaryCondition

trait L2_BoundaryCondition extends L2_Node with L2_Progressable with PrettyPrintable {
  override def progress : L3_BoundaryCondition
}

/// L2_NoBC

case object L2_NoBC extends L2_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "None"
  override def progress = ProgressLocation(L3_NoBC)
}
