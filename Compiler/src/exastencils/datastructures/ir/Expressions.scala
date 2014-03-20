package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.primitives._

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

  import scala.language.implicitConversions
  implicit def op2str(op : Value) : String = op match {
    case Addition       => "+"
    case Subtraction    => "-"
    case Multiplication => "*"
    case Division       => "/"
    case Power          => "**" // FIXME
    case Modulo         => "%"
    case AndAnd         => "&&"
    case OrOr           => "||"
    case EqEq           => "=="
    case NeqNeq         => "!="
    case _              => "FIXME"
  }
}

object UnaryOperators extends Enumeration {
  type UnaryOperators = Value
  val Positive, Negative, Not = Value

  import scala.language.implicitConversions
  implicit def op2str(op : Value) : String = op match {
    case Positive => ""
    case Negative => "-"
    case Not      => "!"
    case _        => "FIXME"
  }
}

trait Access extends Expression
trait Number extends Expression {
  def value : AnyVal
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

case class IntegerConstant(v : Long) extends Number {
  override def cpp = value.toString
  override def value = v
}

case class FloatConstant(v : Double) extends Number {
  override def cpp = value.toString
  override def value = v
}

case class BooleanLiteral(value : Boolean) extends Expression {
  override def cpp = value.toString
}

case class VariableAccess(name : String, dType : Option[Datatype] = None) extends Access {
  override def cpp = name
}

case class ArrayAccess(base : Access, index : Expression) extends Access {
  override def cpp = {
    index match {
      case ind : MultiIndex => base.cpp + ind.cpp
      case ind : Expression => base.cpp + '[' + ind.cpp + ']'
    }
  }
}

case class MultiIndex(index_0 : Expression = new NullExpression, index_1 : Expression = new NullExpression, index_2 : Expression = new NullExpression) extends Expression {
  def this(indices : Array[Expression]) = this(
    if (indices.size > 0) indices(0) else new NullExpression,
    if (indices.size > 1) indices(1) else new NullExpression,
    if (indices.size > 2) indices(2) else new NullExpression)
  def this(indices : Array[Int]) = this(
    (if (indices.size > 0) indices(0) else new NullExpression) : Expression,
    (if (indices.size > 1) indices(1) else new NullExpression) : Expression,
    (if (indices.size > 2) indices(2) else new NullExpression) : Expression)
  def this(left : MultiIndex, right : MultiIndex, f : (Expression, Expression) => Expression) = this(
    if (!left(0).isInstanceOf[NullExpression] && !right(0).isInstanceOf[NullExpression]) { f(left(0), right(0)) } else { new NullExpression },
    if (!left(1).isInstanceOf[NullExpression] && !right(1).isInstanceOf[NullExpression]) { f(left(1), right(1)) } else { new NullExpression },
    if (!left(2).isInstanceOf[NullExpression] && !right(2).isInstanceOf[NullExpression]) { f(left(2), right(2)) } else { new NullExpression })

  override def cpp = {
    ( // compatibility to Harald's code
      "("
      + s"${index_0.cpp}"
      + (if (Knowledge.dimensionality > 1) s", ${index_1.cpp}" else "")
      + (if (Knowledge.dimensionality > 2) s", ${index_2.cpp}" else "")
      + ")");
  }

  def apply(i : Int) : Expression = {
    i match {
      case 0 => index_0
      case 1 => index_1
      case 2 => index_2
    }
  }
}

object DefaultLoopMultiIndex {
  def apply() : MultiIndex = { new MultiIndex("x", "y", "z"); }
}

case class DirectFieldAccess(var fieldOwner : Expression, var field : Field, var slot : Expression, var index : MultiIndex) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FieldAccess\n";

  def expand(collector : StackCollector) : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldOwner, field, slot, Mapping.resolveMultiIdx(field, index));
  }
}

case class FieldAccess(var fieldOwner : Expression, var field : Field, var slot : Expression, var index : MultiIndex) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FieldAccess\n";

  def expand(collector : StackCollector) : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldOwner, field, slot, Mapping.resolveMultiIdx(field, new MultiIndex(index, field.referenceOffset, _ + _)));
  }
}

case class LinearizedFieldAccess(var fieldOwner : Expression, var field : Field, var slot : Expression, var index : Expression) extends Expression {
  override def cpp : String = {
    s"${fieldOwner.cpp}${field.codeName.cpp}[${slot.cpp}][${field.level}]->data[${index.cpp}]";
  }
}

case class MemberAccess(base : Access, varAcc : VariableAccess) extends Access {
  override def cpp = base.cpp + '.' + varAcc.cpp
}

case class DerefAccess(base : Access) extends Access {
  override def cpp = "*(" + base.cpp + ')'
}

case class UnaryExpression(var operator : UnaryOperators.Value, expression : Expression) extends Expression {
  override def cpp = { s"${operator.toString}(${expression.cpp})" }
}

case class BinaryExpression(var operator : BinaryOperators.Value, var left : Expression, var right : Expression) extends Expression {
  override def cpp = {
    s"(${left.cpp} ${BinaryOperators.op2str(operator)} ${right.cpp})";
  }

  //  override def simplify = {
  //    (left, right) match {
  //      case (x : NumericLiteral[_], y : NumericLiteral[_]) => println(x ~ y); this //NumericLiteral(left.asInstanceOf[NumericLiteral[_]].Value + right.Value)
  //    }
  //  }
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
