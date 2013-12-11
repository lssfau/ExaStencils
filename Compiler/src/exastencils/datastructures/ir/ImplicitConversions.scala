package exastencils.datastructures.ir

import scala.language.implicitConversions
import exastencils.datastructures.ir._
import exastencils.datastructures.Node

object ImplicitConversions {
  implicit def StringToStringLiteral(s : String) : StringLiteral = new StringLiteral(s);
  implicit def ExpressionToExpressionStatement(e : Expression) : ExpressionStatement = new ExpressionStatement(e);
  implicit def StringToStatement(s : String) : Statement = new ExpressionStatement(new StringLiteral(s));
}