package exastencils.boundary.l3

import exastencils.base.ProgressLocation
import exastencils.boundary.l4.L4_NeumannBC
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// L3_NeumannBC

object L3_NeumannBC {
  def apply() = new L3_NeumannBC(Knowledge.discr_defaultNeumannOrder)
  def apply(order : Option[Int]) = new L3_NeumannBC(order.getOrElse(Knowledge.discr_defaultNeumannOrder))
}

case class L3_NeumannBC(order : Int) extends L3_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << "Neumann" << '(' << order << ')'
  override def progress = ProgressLocation(L4_NeumannBC(order))
}
