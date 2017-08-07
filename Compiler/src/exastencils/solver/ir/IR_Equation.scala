package exastencils.solver.ir

import exastencils.base.ir._

/// IR_Equation

case class IR_Equation(var lhs : IR_Expression, var rhs : IR_Expression) extends IR_Node
