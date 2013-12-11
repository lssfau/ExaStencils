package exastencils.datastructures.ir

import scala.language.implicitConversions
import exastencils.datastructures.ir._

object ImplicitConversions {
  implicit def StringToStringLiteral(s : String) = new StringLiteral(s);
  implicit def ExpressionToExpressionStatement(e : Expression) = new ExpressionStatement(e);
  //implicit def StringLiteralToExpressionStatement(s : StringLiteral) = new ExpressionStatement(s);
  implicit def StringToStringLiteralInExpressionStatement(s : String) = new ExpressionStatement(new StringLiteral(s));
}