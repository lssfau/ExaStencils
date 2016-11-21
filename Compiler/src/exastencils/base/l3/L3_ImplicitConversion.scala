package exastencils.base.l3

object L3_ImplicitConversion {

  import scala.language.implicitConversions

  // constants

  implicit def NumberToIntegerConstant(n : Int) : L3_IntegerConstant = L3_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) : L3_IntegerConstant = L3_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) : L3_RealConstant = L3_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) : L3_RealConstant = L3_RealConstant(n)

  implicit def BooleanToBooleanConstant(b : Boolean) : L3_BooleanConstant = L3_BooleanConstant(b)

  implicit def StringToStringLiteral(s : String) : L3_StringLiteral = L3_StringLiteral(s)

  // expression -> statement

  implicit def ExpressionToExpressionStatement(e : L3_Expression) : L3_Statement = L3_ExpressionStatement(e)

  // datatype

  implicit def StringToDatatype(s : String) : L3_Datatype = L3_SpecialDatatype(s)
}
