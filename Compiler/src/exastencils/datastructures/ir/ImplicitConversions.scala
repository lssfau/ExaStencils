package exastencils.datastructures.ir

import scala.language.implicitConversions
import exastencils.datastructures.ir._
import exastencils.datastructures.Node

object ImplicitConversions {
  implicit def StringToStringLiteral(s : String) = new StringLiteral(s);
  implicit def ExpressionToExpressionStatement(e : Expression) = new ExpressionStatement(e);
  implicit def StringToStatement(s : String) = (new ExpressionStatement(new StringLiteral(s)) : Statement);
}