package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_Expression

trait L1_Expression extends L1_Node with L1_Progressable with PrettyPrintable {
  def progress : L2_Expression

  def +(other : L1_Expression) = L1_Addition(this, other)
  def -(other : L1_Expression) = L1_Subtraction(this, other)
  def *(other : L1_Expression) = L1_Multiplication(this, other)
  def /(other : L1_Expression) = L1_Division(this, other)

  def Pow(other : L1_Expression) = L1_Power(this, other)
  def Mod(other : L1_Expression) = L1_Modulo(this, other)
  def Modulo(other : L1_Expression) = L1_Modulo(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : L1_Expression) = L1_ElementwiseAddition(this, other)
  def :-(other : L1_Expression) = L1_ElementwiseSubtraction(this, other)
  def :*(other : L1_Expression) = L1_ElementwiseMultiplication(this, other)
  def :/(other : L1_Expression) = L1_ElementwiseDivision(this, other)

  def DotPow(other : L1_Expression) = L1_ElementwisePower(this, other)
  def DotMod(other : L1_Expression) = L1_ElementwiseModulo(this, other)
  def DotModulo(other : L1_Expression) = L1_ElementwiseModulo(this, other)

  def And(other : L1_Expression) = L1_AndAnd(this, other)
  def AndAnd(other : L1_Expression) = L1_AndAnd(this, other)
  def Or(other : L1_Expression) = L1_OrOr(this, other)
  def OrOr(other : L1_Expression) = L1_OrOr(this, other)

  def EqEq(other : L1_Expression) = L1_EqEq(this, other)
  def Neq(other : L1_Expression) = L1_Neq(this, other)
  def <(other : L1_Expression) = L1_Lower(this, other)
  def <=(other : L1_Expression) = L1_LowerEqual(this, other)
  def >(other : L1_Expression) = L1_Greater(this, other)
  def >=(other : L1_Expression) = L1_GreaterEqual(this, other)
}

/// L1_NullExpression

case object L1_NullExpression extends L1_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(L2_NullExpression)
}
