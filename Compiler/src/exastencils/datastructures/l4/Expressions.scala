package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Expression
    extends Node with CppPrettyPrintable {
  override def duplicate : this.type
}

case class StringLiteral(value : String)
    extends Expression {
  override def cpp = value
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class NumericLiteral(value : Number)
    extends Expression {
  override def cpp = value.toString
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class BooleanLiteral(value : Boolean)
    extends Expression {
  override def cpp = value.toString
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Identifier(name : String)
    extends Expression {
  override def cpp = name
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Variable(name : String, Type : Datatype)
    extends Expression {
  override def cpp = name
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Constant(value : Any) extends Expression {
  override def cpp = ""
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class BinaryExpression(operator : String, left : Expression, right : Expression)
    extends Expression {
  override def cpp = left.cpp + operator + right.cpp
  override def duplicate = this.copy(left = Duplicate(left), right = Duplicate(right)).asInstanceOf[this.type]
}

case class FunctionCall(name : String, arguments : Seq[Expression])
    extends Expression {
  override def cpp = name + "FIXME args"
  override def duplicate = this.copy(arguments = Duplicate(arguments)).asInstanceOf[this.type]
}
