package exastencils.boundary.l4

import exastencils.boundary.ir.IR_NeumannBC
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// L4_NeumannBC

object L4_NeumannBC {
  def apply() = new L4_NeumannBC(Knowledge.experimental_NeumannOrder)
  def apply(order : Option[Int]) = new L4_NeumannBC(order.getOrElse(Knowledge.experimental_NeumannOrder))
}

case class L4_NeumannBC(var order : Int) extends L4_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << "Neumann" << '(' << order << ')'
  override def progress = IR_NeumannBC(order)
}
