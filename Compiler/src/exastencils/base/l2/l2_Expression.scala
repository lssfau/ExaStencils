package exastencils.base.l2

import exastencils.base.l3._
import exastencils.prettyprinting._

trait L2_Expression extends L2_Node with L2_Progressable with PrettyPrintable {
  def progress : L3_Expression

  def +(other : L2_Expression) = L2_AdditionExpression(this, other)
  def -(other : L2_Expression) = L2_SubtractionExpression(this, other)
  def *(other : L2_Expression) = L2_MultiplicationExpression(this, other)
  def /(other : L2_Expression) = L2_DivisionExpression(this, other)

  def Pow(other : L2_Expression) = L2_PowerExpression(this, other)
  def Mod(other : L2_Expression) = L2_ModuloExpression(this, other)
  def Modulo(other : L2_Expression) = L2_ModuloExpression(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : L2_Expression) = L2_ElementwiseAdditionExpression(this, other)
  def :-(other : L2_Expression) = L2_ElementwiseSubtractionExpression(this, other)
  def :*(other : L2_Expression) = L2_ElementwiseMultiplicationExpression(this, other)
  def :/(other : L2_Expression) = L2_ElementwiseDivisionExpression(this, other)

  def DotPow(other : L2_Expression) = L2_ElementwisePowerExpression(this, other)
  def DotMod(other : L2_Expression) = L2_ElementwiseModuloExpression(this, other)
  def DotModulo(other : L2_Expression) = L2_ElementwiseModuloExpression(this, other)

  def And(other : L2_Expression) = L2_AndAndExpression(this, other)
  def AndAnd(other : L2_Expression) = L2_AndAndExpression(this, other)
  def Or(other : L2_Expression) = L2_OrOrExpression(this, other)
  def OrOr(other : L2_Expression) = L2_OrOrExpression(this, other)

  def EqEq(other : L2_Expression) = L2_EqEqExpression(this, other)
  def Neq(other : L2_Expression) = L2_NeqExpression(this, other)
  def <(other : L2_Expression) = L2_LowerExpression(this, other)
  def <=(other : L2_Expression) = L2_LowerEqualExpression(this, other)
  def >(other : L2_Expression) = L2_GreaterExpression(this, other)
  def >=(other : L2_Expression) = L2_GreaterEqualExpression(this, other)
}

case object L2_NullExpression extends L2_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = L3_NullExpression
}
