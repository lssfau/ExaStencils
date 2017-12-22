package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.optimization.l4.L4_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// L4_Equation

case class L4_Equation(var lhs : L4_Expression, var rhs : L4_Expression) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs
  override def progress = ProgressLocation(IR_Equation(lhs.progress, rhs.progress))

  def asZeroEquation() : L4_Expression = {
    val zeroEq : L4_Expression = Duplicate(lhs - rhs)
    L4_GeneralSimplifyWrapper.process(zeroEq)
  }
}
