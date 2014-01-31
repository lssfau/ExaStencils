package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.ir._

import harald.dsl.ParameterInfo // TODO: to be removed

trait Expression extends Node with CppPrettyPrintable {
  def ~(exp : Expression) : ConcatenationExpression = {
    new ConcatenationExpression(ListBuffer(this, exp))
  }

  // FIXME: this is currently required for interop with Harald's code; to be removed after integration
  def evaluate(para : ListBuffer[ParameterInfo]) : Int = 0
}

case class NullExpression() extends Expression {
  def cpp : String = ""
}

case class ConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def cpp = expressions.map(e => e.cpp).mkString(" ")
  override def ~(exp : Expression) : ConcatenationExpression = {
    expressions += exp
    this
  }
}

case class StringLiteral(value : String) extends Expression {
  override def cpp = value
}

case class NumericLiteral(value : Number /*FIXME: we should think about using a scala alternative instead of java datatypes*/ ) extends Expression {
  override def cpp = value.toString
}

case class BooleanLiteral(value : Boolean) extends Expression {
  override def cpp = value.toString
}

case class Identifier(name : String) extends Expression {
  override def cpp = name
}

case class Variable(var datatype : Datatype, name : String) extends Expression {
  // FIXME: treat static arrays
  override def cpp = name
}

case class Constant(value : Any) extends Expression {
  override def cpp = ""
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  override def cpp = {
    s"(${left.cpp} $operator ${right.cpp})";
  }
}

case class FunctionCallExpression(var name : Expression, var arguments : ListBuffer[Expression /* FIXME: more specialization*/ ]) extends Expression {
  override def cpp : String = {
    return (s"${name.cpp}(" + arguments.map(arg => arg.cpp).mkString(", ") + ")");
  }
}

case class MemberFunctionCallExpression(var objectName : Expression, var name : Expression, var arguments : ListBuffer[Expression /* FIXME: more specialization*/ ]) extends Expression {
  override def cpp : String = {
    return (s"${objectName.cpp}.${name.cpp}(" + arguments.map(arg => arg.cpp).mkString(", ") + ")");
  }
}

case class TernaryConditionExpression(var condition : Expression, var trueBody : Expression, var falseBody : Expression) extends Expression {
  def cpp : String = {
    (s"((${condition.cpp}) ? (${trueBody.cpp}) : (${falseBody.cpp}))");
  }
}
