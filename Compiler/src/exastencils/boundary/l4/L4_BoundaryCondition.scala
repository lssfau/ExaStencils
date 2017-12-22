package exastencils.boundary.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.boundary.ir._
import exastencils.prettyprinting._

/// L4_BoundaryCondition

trait L4_BoundaryCondition extends L4_Node with L4_Progressable with PrettyPrintable {
  override def progress : IR_BoundaryCondition
}

/// L4_NoBC

case object L4_NoBC extends L4_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "None"
  override def progress = ProgressLocation(IR_NoBC)
}
