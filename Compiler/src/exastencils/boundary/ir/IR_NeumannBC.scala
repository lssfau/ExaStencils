package exastencils.boundary.ir

import exastencils.config.Knowledge

/// IR_NeumannBC

object IR_NeumannBC {
  def apply() = new IR_NeumannBC(Knowledge.experimental_NeumannOrder)
}

case class IR_NeumannBC(var order : Int) extends IR_BoundaryCondition {}
