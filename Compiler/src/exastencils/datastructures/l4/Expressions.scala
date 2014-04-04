package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Expression extends Node
trait Number extends Expression {
  def value : AnyVal
}

case class StringConstant(value : String) extends Expression

case class IntegerConstant(v : Long) extends Number {
  override def value = v
}

case class FloatConstant(v : Double) extends Number {
  override def value = v
}

case class BooleanConstant(value : Boolean) extends Expression

case class Identifier(name : String, level : Option[LevelSpecification]) extends Expression

case class Variable(identifier : Identifier, Type : Datatype) extends Expression

case class BinaryExpression(operator : String, var left : Expression, var right : Expression) extends Expression

case class BooleanExpression(operator : String, var left : Expression, var right : Expression) extends Expression

case class FunctionCallExpression(identifier : Identifier, var arguments : List[Expression]) extends Expression
