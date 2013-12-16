package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.datastructures.ir._

trait Expression extends Node with CppPrettyPrintable {
  override def duplicate : this.type
}

case class StringLiteral(value : String) extends Expression {
  override def cpp = value
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class NumericLiteral(value : Number) extends Expression {
  override def cpp = value.toString
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class BooleanLiteral(value : Boolean) extends Expression {
  override def cpp = value.toString
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Identifier(name : String) extends Expression {
  override def cpp = name
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Variable(var datatype : Datatype, name : String) extends Expression {
  override def cpp = name
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Constant(value : Any) extends Expression {
  override def cpp = ""
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class BinaryExpression(operator : String, var left : Expression, var right : Expression) extends Expression {
  override def cpp = left.cpp + operator + right.cpp
  override def duplicate = this.copy(left = Duplicate(left), right = Duplicate(right)).asInstanceOf[this.type]
}

case class FunctionCall(name : String, var arguments : List[Expression]) extends Expression {
  override def cpp = name + "FIXME args"
  override def duplicate = this.copy(arguments = Duplicate(arguments)).asInstanceOf[this.type]
}