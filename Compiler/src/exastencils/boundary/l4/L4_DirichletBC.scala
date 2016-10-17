package exastencils.boundary.l4

import exastencils.base.l4.L4_Expression
import exastencils.boundary.ir.IR_DirichletBC
import exastencils.prettyprinting.PpStream

/// L4_DirichletBC

case class L4_DirichletBC(boundaryValue : L4_Expression) extends L4_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryValue
  override def progress = IR_DirichletBC(boundaryValue.progress)
}
