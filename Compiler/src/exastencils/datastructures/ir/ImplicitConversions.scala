package exastencils.datastructures.ir

import scala.language.implicitConversions
import exastencils.datastructures.ir._
import exastencils.datastructures._
import scala.collection.mutable.ListBuffer

object ImplicitConversions {
  implicit def StringToStringLiteral(s : String) = new StringConstant(s);
  implicit def ExpressionToExpressionStatement(e : Expression) = new ExpressionStatement(e);
  implicit def StringToStatement(s : String) = (new ExpressionStatement(new StringConstant(s)) : Statement);

  implicit def StringToDatatype(s : String) = (new SpecialDatatype(s) : Datatype);

  implicit def NumberToIntegerConstant(n : Int) = IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) = IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) = FloatConstant(n)
  implicit def NumberToFloatConstant(n : Double) = FloatConstant(n)
}