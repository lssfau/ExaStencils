package exastencils.base.l1

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.optimization.l1.L1_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// L1_Equation

case class L1_Equation(var lhs : L1_Expression, var rhs : L1_Expression) extends L1_Node with PrettyPrintable with L1_Progressable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs
  override def progress = L2_Equation(lhs.progress, rhs.progress)

  def asZeroEquation() : L1_Expression = {
    val zeroEq : L1_Expression = Duplicate(lhs - rhs)
    L1_GeneralSimplifyWrapper.process(zeroEq)
  }
}
