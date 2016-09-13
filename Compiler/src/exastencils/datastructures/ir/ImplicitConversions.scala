package exastencils.datastructures.ir

import exastencils.base.ir._

object ImplicitConversions {

  import scala.language.implicitConversions

  implicit def StringToStringLiteral(s : String) = new StringLiteral(s);
  implicit def ExpressionToExpressionStatement(e : Expression) = new ExpressionStatement(e);
  implicit def StringToStatement(s : String) = (new ExpressionStatement(new StringLiteral(s)) : Statement);

  implicit def StringToDatatype(s : String) = (new IR_SpecialDatatype(s) : IR_Datatype);

  implicit def BooleanToBooleanConstant(b : Boolean) = BooleanConstant(b)

  implicit def NumberToIntegerConstant(n : Int) = IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) = IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) = FloatConstant(n)
  implicit def NumberToFloatConstant(n : Double) = FloatConstant(n)
}
