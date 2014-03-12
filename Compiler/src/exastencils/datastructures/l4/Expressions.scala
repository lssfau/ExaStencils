package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Expression extends Node

case class StringLiteral(value : String) extends Expression

case class NumericLiteral[T : Numeric](value : T) extends Expression

case class BooleanLiteral(value : Boolean) extends Expression

case class Identifier(name : String) extends Expression

case class Variable(identifier : Identifier, Type : Datatype) extends Expression

case class BinaryExpression(operator : String, var left : Expression, var right : Expression) extends Expression

case class BooleanExpression(operator : String, var left : Expression, var right : Expression) extends Expression

case class FunctionCallExpression(name : String, var arguments : List[Expression]) extends Expression
