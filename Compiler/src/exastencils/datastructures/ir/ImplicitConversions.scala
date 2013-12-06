package exastencils.datastructures.ir

import exastencils.datastructures.ir._

object ImplicitConversions {
  implicit def StringToStringLiteral(s : String) = new StringLiteral(s)
  implicit def ExpressionToExpressionStatement(e : Expression) = new ExpressionStatement(e)
}