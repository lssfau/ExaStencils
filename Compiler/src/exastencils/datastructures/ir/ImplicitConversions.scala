package exastencils.datastructures.ir

import exastencils.base.ir._

object ImplicitConversions {

  import scala.language.implicitConversions

  implicit def StringToStringLiteral(s : String) = new IR_StringLiteral(s);
  implicit def ExpressionToExpressionStatement(e : IR_Expression) = new IR_ExpressionStatement(e);
  implicit def StringToStatement(s : String) = (new IR_ExpressionStatement(new IR_StringLiteral(s)) : IR_Statement);

  implicit def StringToDatatype(s : String) = (new IR_SpecialDatatype(s) : IR_Datatype);

  implicit def BooleanToBooleanConstant(b : Boolean) = IR_BooleanConstant(b)

  implicit def NumberToIntegerConstant(n : Int) = IR_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) = IR_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) = IR_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) = IR_RealConstant(n)
}
