package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Expression
    extends Node with CppPrettyPrintable {

  override def duplicate : this.type
}

case class Variable(name : String, Type : Datatype)
    extends Expression with CppPrettyPrintable {
  def cpp = { name }

  override def duplicate = { this.copy().asInstanceOf[this.type] }
}

case class BooleanExpression(left : Expression, operator : String, right : Expression)
    extends Expression {
  def cpp = { left.cpp + operator + right.cpp }

  override def duplicate = { this.copy(left = Duplicate(left), right = Duplicate(right)).asInstanceOf[this.type] }
}
case class ConstantExpression(value : Any) extends Expression {
  def cpp = { "" }

  override def duplicate = { this.copy().asInstanceOf[this.type] }
}