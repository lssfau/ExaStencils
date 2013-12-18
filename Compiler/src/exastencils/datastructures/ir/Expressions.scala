package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.ir._

trait Expression extends Node with CppPrettyPrintable {
  override def duplicate : this.type
  def ~(exp : Expression) : ConcatenationExpression = {
    new ConcatenationExpression(ListBuffer(this, exp))
  }
}

case class ConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def duplicate = this.copy(expressions = Duplicate(expressions)).asInstanceOf[this.type]
  override def cpp = expressions.map(e => e.cpp).mkString(" ")
  override def ~(exp : Expression) : ConcatenationExpression = {
    expressions += exp
    this
  }
}

case class StringLiteral(value : String) extends Expression {
  override def cpp = value
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class NumericLiteral(value : Number /*FIXME: we should think about using a scala alternative instead of java datatypes*/ ) extends Expression {
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

case class FunctionCallExpression(name : String, var arguments : List[Expression]) extends Expression {
  override def cpp = name + "FIXME args"
  override def duplicate = this.copy(arguments = Duplicate(arguments)).asInstanceOf[this.type]
}

case class TernaryConditionExpression(var condition : Expression, var trueBody : Expression, var falseBody : Expression) extends Expression {
  override def duplicate = { this.copy(condition = Duplicate(condition), trueBody = Duplicate(trueBody), falseBody = Duplicate(falseBody)).asInstanceOf[this.type] }

  def cpp : String = {
    (s"((${condition.cpp}) ? (${trueBody.cpp}) : (${falseBody.cpp}))");
  }
}
