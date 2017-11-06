package exastencils.boundary.l1

import exastencils.base.l1.L1_Expression
import exastencils.boundary.l2.L2_DirichletBC
import exastencils.prettyprinting.PpStream

/// L1_DirichletBC

case class L1_DirichletBC(boundaryValue : L1_Expression) extends L1_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryValue
  override def progress = L2_DirichletBC(boundaryValue.progress)
}
