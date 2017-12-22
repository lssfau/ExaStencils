package exastencils.boundary.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_Expression
import exastencils.boundary.l3.L3_DirichletBC
import exastencils.prettyprinting.PpStream

/// L2_DirichletBC

case class L2_DirichletBC(boundaryValue : L2_Expression) extends L2_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryValue
  override def progress = ProgressLocation(L3_DirichletBC(boundaryValue.progress))
}
