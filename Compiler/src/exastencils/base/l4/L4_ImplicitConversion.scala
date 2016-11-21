package exastencils.base.l4

object L4_ImplicitConversion {

  import scala.language.implicitConversions

  // constants

  implicit def NumberToIntegerConstant(n : Int) : L4_IntegerConstant = L4_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) : L4_IntegerConstant = L4_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) : L4_RealConstant = L4_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) : L4_RealConstant = L4_RealConstant(n)

  implicit def BooleanToBooleanConstant(b : Boolean) : L4_BooleanConstant = L4_BooleanConstant(b)

  implicit def StringToStringLiteral(s : String) : L4_StringLiteral = L4_StringLiteral(s)

  // expression -> statement

  implicit def ExpressionToExpressionStatement(e : L4_Expression) : L4_Statement = L4_ExpressionStatement(e)

  // datatype

  implicit def StringToDatatype(s : String) : L4_Datatype = L4_SpecialDatatype(s)
}
