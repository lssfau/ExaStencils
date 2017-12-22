package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.optimization.l3.L3_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// L3_Equation

case class L3_Equation(var lhs : L3_Expression, var rhs : L3_Expression) extends L3_Node with PrettyPrintable with L3_Progressable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs
  override def progress = ProgressLocation(L4_Equation(lhs.progress, rhs.progress))

  def asZeroEquation() : L3_Expression = {
    val zeroEq : L3_Expression = Duplicate(lhs - rhs)
    L3_GeneralSimplifyWrapper.process(zeroEq)
  }
}
