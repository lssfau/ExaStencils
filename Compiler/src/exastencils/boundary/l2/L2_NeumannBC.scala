package exastencils.boundary.l2

import exastencils.boundary.l3.L3_NeumannBC
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// L2_NeumannBC

object L2_NeumannBC {
  def apply() = new L2_NeumannBC(Knowledge.experimental_NeumannOrder)
  def apply(order : Option[Int]) = new L2_NeumannBC(order.getOrElse(Knowledge.experimental_NeumannOrder))
}

case class L2_NeumannBC(order : Int) extends L2_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << "Neumann" << '(' << order << ')'
  override def progress = L3_NeumannBC(order)
}
