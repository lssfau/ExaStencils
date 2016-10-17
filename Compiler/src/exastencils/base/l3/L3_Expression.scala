package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting._

trait L3_Expression extends L3_Node with L3_Progressable with PrettyPrintable {
  def progress : L4_Expression

  def +(other : L3_Expression) = L3_Addition(this, other)
  def -(other : L3_Expression) = L3_Subtraction(this, other)
  def *(other : L3_Expression) = L3_Multiplication(this, other)
  def /(other : L3_Expression) = L3_Division(this, other)

  def Pow(other : L3_Expression) = L3_Power(this, other)
  def Mod(other : L3_Expression) = L3_Modulo(this, other)
  def Modulo(other : L3_Expression) = L3_Modulo(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : L3_Expression) = L3_ElementwiseAddition(this, other)
  def :-(other : L3_Expression) = L3_ElementwiseSubtraction(this, other)
  def :*(other : L3_Expression) = L3_ElementwiseMultiplication(this, other)
  def :/(other : L3_Expression) = L3_ElementwiseDivision(this, other)

  def DotPow(other : L3_Expression) = L3_ElementwisePower(this, other)
  def DotMod(other : L3_Expression) = L3_ElementwiseModulo(this, other)
  def DotModulo(other : L3_Expression) = L3_ElementwiseModulo(this, other)

  def And(other : L3_Expression) = L3_AndAnd(this, other)
  def AndAnd(other : L3_Expression) = L3_AndAnd(this, other)
  def Or(other : L3_Expression) = L3_OrOr(this, other)
  def OrOr(other : L3_Expression) = L3_OrOr(this, other)

  def EqEq(other : L3_Expression) = L3_EqEq(this, other)
  def Neq(other : L3_Expression) = L3_Neq(this, other)
  def <(other : L3_Expression) = L3_Lower(this, other)
  def <=(other : L3_Expression) = L3_LowerEqual(this, other)
  def >(other : L3_Expression) = L3_Greater(this, other)
  def >=(other : L3_Expression) = L3_GreaterEqual(this, other)
}

case object L3_NullExpression extends L3_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = L4_NullExpression
}
