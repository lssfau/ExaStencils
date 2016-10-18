package exastencils.boundary.l3

import exastencils.base.l3.L3_Expression
import exastencils.boundary.l4.L4_DirichletBC
import exastencils.prettyprinting.PpStream

/// L3_DirichletBC

case class L3_DirichletBC(boundaryValue : L3_Expression) extends L3_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryValue
  override def progress = L4_DirichletBC(boundaryValue.progress)
}
