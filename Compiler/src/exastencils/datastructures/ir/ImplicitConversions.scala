package exastencils.datastructures.ir

object ImplicitConversions {
  implicit def StringToStringLiteral(s : String) = new StringConstant(s);
  implicit def ExpressionToExpressionStatement(e : Expression) = new ExpressionStatement(e);
  implicit def StringToStatement(s : String) = (new ExpressionStatement(new StringConstant(s)) : Statement);

  implicit def StringToDatatype(s : String) = (new SpecialDatatype(s) : Datatype);

  implicit def BooleanToBooleanConstant(b : Boolean) = BooleanConstant(b)

  implicit def NumberToIntegerConstant(n : Int) = IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) = IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) = FloatConstant(n)
  implicit def NumberToFloatConstant(n : Double) = FloatConstant(n)
}