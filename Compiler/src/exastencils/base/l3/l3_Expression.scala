package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting._

trait L3_Expression extends L3_Node with L3_Progressable with PrettyPrintable {
  def progress : L4_Expression

  def +(other : L3_Expression) = L3_AdditionExpression(this, other)
  def -(other : L3_Expression) = L3_SubtractionExpression(this, other)
  def *(other : L3_Expression) = L3_MultiplicationExpression(this, other)
  def /(other : L3_Expression) = L3_DivisionExpression(this, other)

  def Pow(other : L3_Expression) = L3_PowerExpression(this, other)
  def Mod(other : L3_Expression) = L3_ModuloExpression(this, other)
  def Modulo(other : L3_Expression) = L3_ModuloExpression(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : L3_Expression) = L3_ElementwiseAdditionExpression(this, other)
  def :-(other : L3_Expression) = L3_ElementwiseSubtractionExpression(this, other)
  def :*(other : L3_Expression) = L3_ElementwiseMultiplicationExpression(this, other)
  def :/(other : L3_Expression) = L3_ElementwiseDivisionExpression(this, other)

  def DotPow(other : L3_Expression) = L3_ElementwisePowerExpression(this, other)
  def DotMod(other : L3_Expression) = L3_ElementwiseModuloExpression(this, other)
  def DotModulo(other : L3_Expression) = L3_ElementwiseModuloExpression(this, other)

  def And(other : L3_Expression) = L3_AndAndExpression(this, other)
  def AndAnd(other : L3_Expression) = L3_AndAndExpression(this, other)
  def Or(other : L3_Expression) = L3_OrOrExpression(this, other)
  def OrOr(other : L3_Expression) = L3_OrOrExpression(this, other)

  def EqEq(other : L3_Expression) = L3_EqEqExpression(this, other)
  def Neq(other : L3_Expression) = L3_NeqExpression(this, other)
  def <(other : L3_Expression) = L3_LowerExpression(this, other)
  def <=(other : L3_Expression) = L3_LowerEqualExpression(this, other)
  def >(other : L3_Expression) = L3_GreaterExpression(this, other)
  def >=(other : L3_Expression) = L3_GreaterEqualExpression(this, other)
}

case object L3_NullExpression extends L3_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = L4_NullExpression
}
