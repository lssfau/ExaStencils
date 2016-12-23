package exastencils.boundary.l3

import exastencils.base.l3._
import exastencils.boundary.l4._
import exastencils.prettyprinting._

/// L3_BoundaryCondition

trait L3_BoundaryCondition extends L3_Node with L3_Progressable with PrettyPrintable {
  override def progress : L4_BoundaryCondition
}

/// L3_NoBC

case object L3_NoBC extends L3_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "None"
  override def progress = L4_NoBC
}
