package exastencils.boundary.l4

import exastencils.base.l4.L4_Expression
import exastencils.boundary.ir.IR_DirichletBC
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// L4_DirichletBC

object L4_DirichletBC {
  def apply(boundaryValue : L4_Expression) = new L4_DirichletBC(boundaryValue, Knowledge.discr_defaultDirichletOrder)
}

case class L4_DirichletBC(var boundaryValue : L4_Expression, var order : Int) extends L4_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryValue
  override def progress = IR_DirichletBC(boundaryValue.progress, order)
}
