package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.ir._

trait Expression extends Node with CppPrettyPrintable {
  def ~(exp : Expression) : ConcatenationExpression = {
    new ConcatenationExpression(ListBuffer(this, exp))
  }
}

trait Access extends Expression

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

case class NumericLiteral[T : Numeric](value : T) extends Expression {
  override def cpp = value.toString
}

case class BooleanLiteral(value : Boolean) extends Expression {
  override def cpp = value.toString
}

case class VariableAccess(name : String, dType : Option[Datatype] = None) extends Access {
  override def cpp = name
}

case class ArrayAccess(base : Access, indices : Array[Expression]) extends Access {
  override def cpp = base.cpp + indices.map({ expr =>
    '[' + expr.cpp + ']'
  }).mkString
}

case class FieldAccess_(base : Access, varAcc : VariableAccess) extends Access {
  override def cpp = base.cpp + '.' + varAcc.cpp
}

case class DerefAccess(base : Access) extends Access {
  override def cpp = "*(" + base.cpp + ')'
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
