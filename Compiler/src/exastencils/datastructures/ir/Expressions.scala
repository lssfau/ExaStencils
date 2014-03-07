package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.ir._

trait Expression extends Node with CppPrettyPrintable {
  def ~(exp : Expression) : ConcatenationExpression = {
    new ConcatenationExpression(ListBuffer(this, exp))
  }

  import BinaryOperators._
  def +(other : Expression) = new BinaryExpression(Addition, this, other)
  def -(other : Expression) = new BinaryExpression(Subtraction, this, other)
  def *(other : Expression) = new BinaryExpression(Multiplication, this, other)
  def /(other : Expression) = new BinaryExpression(Division, this, other)
  def Pow(other : Expression) = new BinaryExpression(Power, this, other)
  def Mod(other : Expression) = new BinaryExpression(Modulo, this, other)
  def And(other : Expression) = new BinaryExpression(AndAnd, this, other)
  def Or(other : Expression) = new BinaryExpression(OrOr, this, other)
  def Eq(other : Expression) = new BinaryExpression(EqEq, this, other)
  def IsNeq(other : Expression) = new BinaryExpression(NeqNeq, this, other)
  
  def simplify : Expression = this
}

object BinaryOperators extends Enumeration {
  type BinaryOperators = Value
  val Addition, Subtraction, Multiplication, Division, Power, Modulo, AndAnd, OrOr, EqEq, NeqNeq = Value
}

object UnaryOperators extends Enumeration {
  type UnaryOperators = Value
  val Positive, Negative, Not = Value
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

case class ArrayAccess(base : Access, index : Expression) extends Access {
  override def cpp = index match {
    case ind : MultiIndex => base.cpp + ind.cpp
    case ind : Expression => base.cpp + '[' + ind.cpp + ']'
  }
}

case class MultiIndex(ind1 : Expression, ind2 : Expression, ind3 : Expression) extends Expression {
  override def cpp = '[' + ind1.cpp + "][" + ind2.cpp + "][" + ind3.cpp + ']'
}

case class MemberAccess(base : Access, varAcc : VariableAccess) extends Access {
  override def cpp = base.cpp + '.' + varAcc.cpp
}

case class DerefAccess(base : Access) extends Access {
  override def cpp = "*(" + base.cpp + ')'
}

case class Constant(value : Any) extends Expression {
  override def cpp = ""
}

case class UnaryExpression(var operator : UnaryOperators.Value, expression : Expression) extends Expression {
  override def cpp = { s"${operator.toString}(${expression.cpp})" }
}

case class BinaryExpression(var operator : BinaryOperators.Value, var left : Expression, var right : Expression) extends Expression {
  override def cpp = {
    s"(${left.cpp} $operator ${right.cpp})";
  }
  
  override def simplify = {
     (left, right) match {
       case (x : NumericLiteral[_] , y : NumericLiteral[_]) => println(x ~ y); this //NumericLiteral(left.asInstanceOf[NumericLiteral[_]].Value + right.Value)
    }
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
