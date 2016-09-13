package exastencils.base.ir

import exastencils.prettyprinting._

trait IR_Expression extends IR_Node with PrettyPrintable {

  //TODO: trait IR_Expression extends IR_Node with PrettyPrintable {}

  def +(other : IR_Expression) = IR_AdditionExpression(this, other)
  def -(other : IR_Expression) = IR_SubtractionExpression(this, other)
  def *(other : IR_Expression) = IR_MultiplicationExpression(this, other)
  def /(other : IR_Expression) = IR_DivisionExpression(this, other)

  def Pow(other : IR_Expression) = IR_PowerExpression(this, other)
  def Mod(other : IR_Expression) = IR_ModuloExpression(this, other)
  def Modulo(other : IR_Expression) = IR_ModuloExpression(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : IR_Expression) = IR_ElementwiseAdditionExpression(this, other)
  def :-(other : IR_Expression) = IR_ElementwiseSubtractionExpression(this, other)
  def :*(other : IR_Expression) = IR_ElementwiseMultiplicationExpression(this, other)
  def :/(other : IR_Expression) = IR_ElementwiseDivisionExpression(this, other)

  def DotPow(other : IR_Expression) = IR_ElementwisePowerExpression(this, other)
  def DotMod(other : IR_Expression) = IR_ElementwiseModuloExpression(this, other)
  def DotModulo(other : IR_Expression) = IR_ElementwiseModuloExpression(this, other)

  def And(other : IR_Expression) = IR_AndAndExpression(this, other)
  def AndAnd(other : IR_Expression) = IR_AndAndExpression(this, other)
  def Or(other : IR_Expression) = IR_OrOrExpression(this, other)
  def OrOr(other : IR_Expression) = IR_OrOrExpression(this, other)

  def EqEq(other : IR_Expression) = IR_EqEqExpression(this, other)
  def Neq(other : IR_Expression) = IR_NeqExpression(this, other)
  def <(other : IR_Expression) = IR_LowerExpression(this, other)
  def <=(other : IR_Expression) = IR_LowerEqualExpression(this, other)
  def >(other : IR_Expression) = IR_GreaterExpression(this, other)
  def >=(other : IR_Expression) = IR_GreaterEqualExpression(this, other)

  def <<(other : IR_Expression) = IR_LeftShiftExpression(this, other)
}

case object IR_NullExpression extends IR_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
}
