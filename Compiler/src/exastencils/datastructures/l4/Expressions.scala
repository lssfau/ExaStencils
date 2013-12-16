package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Expression extends Node {
  override def duplicate : this.type
}

case class StringLiteral(value : String) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class NumericLiteral(value : Number) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class BooleanLiteral(value : Boolean) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Identifier(name : String) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Variable(name : String, Type : Datatype) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Constant(value : Any) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class BinaryExpression(operator : String, var left : Expression, var right : Expression) extends Expression {
  override def duplicate = this.copy(left = Duplicate(left), right = Duplicate(right)).asInstanceOf[this.type]
}

case class FunctionCall(name : String, var arguments : List[Expression]) extends Expression {
  override def duplicate = this.copy(arguments = Duplicate(arguments)).asInstanceOf[this.type]
}
