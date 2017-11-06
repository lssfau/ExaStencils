package exastencils.boundary.l1

import exastencils.boundary.l2.L2_NeumannBC
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// L1_NeumannBC

object L1_NeumannBC {
  def apply() = new L1_NeumannBC(Knowledge.discr_defaultNeumannOrder)
  def apply(order : Option[Int]) = new L1_NeumannBC(order.getOrElse(Knowledge.discr_defaultNeumannOrder))
}

case class L1_NeumannBC(order : Int) extends L1_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << "Neumann" << '(' << order << ')'
  override def progress = L2_NeumannBC(order)
}
