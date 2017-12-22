package exastencils.base.l2

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.optimization.l2.L2_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// L2_Equation

case class L2_Equation(var lhs : L2_Expression, var rhs : L2_Expression) extends L2_Node with PrettyPrintable with L2_Progressable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs
  override def progress = ProgressLocation(L3_Equation(lhs.progress, rhs.progress))

  def asZeroEquation() : L2_Expression = {
    val zeroEq : L2_Expression = Duplicate(lhs - rhs)
    L2_GeneralSimplifyWrapper.process(zeroEq)
  }
}
