package exastencils.boundary.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1._
import exastencils.boundary.l2._
import exastencils.prettyprinting._

/// L1_BoundaryCondition

trait L1_BoundaryCondition extends L1_Node with L1_Progressable with PrettyPrintable {
  override def progress : L2_BoundaryCondition
}

/// L1_NoBC

case object L1_NoBC extends L1_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "None"
  override def progress = ProgressLocation(L2_NoBC)
}
