package exastencils.boundary.l2

import exastencils.base.ProgressLocation
import exastencils.boundary.l3.L3_NeumannBC
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// L2_NeumannBC

object L2_NeumannBC {
  def apply() = new L2_NeumannBC(Knowledge.discr_defaultNeumannOrder)
  def apply(order : Option[Int]) = new L2_NeumannBC(order.getOrElse(Knowledge.discr_defaultNeumannOrder))
}

case class L2_NeumannBC(order : Int) extends L2_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << "Neumann" << '(' << order << ')'
  override def progress = ProgressLocation(L3_NeumannBC(order))
}
