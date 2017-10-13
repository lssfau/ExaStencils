package exastencils.base.ir

import exastencils.core.Duplicate
import exastencils.optimization.ir.IR_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// IR_Equation

case class IR_Equation(var lhs : IR_Expression, var rhs : IR_Expression) extends IR_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs

  def asZeroEquation() : IR_Expression = {
    val zeroEq : IR_Expression = Duplicate(lhs - rhs)
    IR_GeneralSimplifyWrapper.process(zeroEq)
  }
}
