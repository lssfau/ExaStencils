package exastencils.base.l4

import exastencils.base.ir._
import exastencils.prettyprinting._

trait L4_Expression extends L4_Node with L4_Progressable with PrettyPrintable {
  def progress : IR_Expression

  def +(other : L4_Expression) = L4_AdditionExpression(this, other)
  def -(other : L4_Expression) = L4_SubtractionExpression(this, other)
  def *(other : L4_Expression) = L4_MultiplicationExpression(this, other)
  def /(other : L4_Expression) = L4_DivisionExpression(this, other)

  def Pow(other : L4_Expression) = L4_PowerExpression(this, other)
  def Mod(other : L4_Expression) = L4_ModuloExpression(this, other)
  def Modulo(other : L4_Expression) = L4_ModuloExpression(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : L4_Expression) = L4_ElementwiseAdditionExpression(this, other)
  def :-(other : L4_Expression) = L4_ElementwiseSubtractionExpression(this, other)
  def :*(other : L4_Expression) = L4_ElementwiseMultiplicationExpression(this, other)
  def :/(other : L4_Expression) = L4_ElementwiseDivisionExpression(this, other)

  def DotPow(other : L4_Expression) = L4_ElementwisePowerExpression(this, other)
  def DotMod(other : L4_Expression) = L4_ElementwiseModuloExpression(this, other)
  def DotModulo(other : L4_Expression) = L4_ElementwiseModuloExpression(this, other)

  def And(other : L4_Expression) = L4_AndAndExpression(this, other)
  def AndAnd(other : L4_Expression) = L4_AndAndExpression(this, other)
  def Or(other : L4_Expression) = L4_OrOrExpression(this, other)
  def OrOr(other : L4_Expression) = L4_OrOrExpression(this, other)

  def EqEq(other : L4_Expression) = L4_EqEqExpression(this, other)
  def Neq(other : L4_Expression) = L4_NeqExpression(this, other)
  def <(other : L4_Expression) = L4_LowerExpression(this, other)
  def <=(other : L4_Expression) = L4_LowerEqualExpression(this, other)
  def >(other : L4_Expression) = L4_GreaterExpression(this, other)
  def >=(other : L4_Expression) = L4_GreaterEqualExpression(this, other)
}

case object L4_NullExpression extends L4_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = IR_NullExpression
}
