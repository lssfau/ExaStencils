package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._

trait Expression extends Node with CppPrettyPrintable {
  def ~(exp : Expression) : ConcatenationExpression = {
    new ConcatenationExpression(ListBuffer(this, exp))
  }
  def ~~(exp : Expression) : SpacedConcatenationExpression = {
    new SpacedConcatenationExpression(ListBuffer(this, exp))
  }

  import BinaryOperators._
  def +(other : Expression) = new AdditionExpression(this, other)
  def -(other : Expression) = new SubtractionExpression(this, other)
  def *(other : Expression) = new MultiplicationExpression(this, other)
  def /(other : Expression) = new DivisionExpression(this, other)
  def Pow(other : Expression) = new PowerExpression(this, other)
  def Mod(other : Expression) = new ModuloExpression(this, other)
  def Modulo(other : Expression) = new ModuloExpression(this, other)
  def And(other : Expression) = new AndAndExpression(this, other)
  def AndAnd(other : Expression) = new AndAndExpression(this, other)
  def Or(other : Expression) = new OrOrExpression(this, other)
  def OrOr(other : Expression) = new OrOrExpression(this, other)
  def EqEq(other : Expression) = new EqEqExpression(this, other)
  def IsNeq(other : Expression) = new NeqNeqExpression(this, other)
  def <(other : Expression) = new LowerExpression(this, other)
  def <=(other : Expression) = new LowerEqualExpression(this, other)
  def >(other : Expression) = new GreaterExpression(this, other)
  def >=(other : Expression) = new GreaterEqualExpression(this, other)

  def simplify : Expression = this

  def cppsb(sb : StringBuilder) : Unit = {
    sb.append(this.cpp)
  }
}

object BinaryOperators extends Enumeration {
  type BinaryOperators = Value
  val Addition, Subtraction, Multiplication, Division, Power, Modulo, AndAnd, OrOr, EqEq, NeqNeq, Lower, LowerEqual, Greater, GreaterEqual = Value

  exastencils.core.Duplicate.registerImmutable(this.getClass())

  import scala.language.implicitConversions
  implicit def op2str(op : BinaryOperators) : String = op match {
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
    case Lower          => "<"
    case LowerEqual     => "<="
    case Greater        => ">"
    case GreaterEqual   => ">="
    case _              => "ERROR: Unresolvable BinaryOperator " + op
  }

  implicit def str2op(op : String) : Value = op match {
    case "+"  => Addition
    case "-"  => Subtraction
    case "*"  => Multiplication
    case "/"  => Division
    case "**" => Power // FIXME  
    case "%"  => Modulo
    case "&&" => AndAnd
    case "||" => OrOr
    case "==" => EqEq
    case "!=" => NeqNeq
    case "<"  => Lower
    case "<=" => LowerEqual
    case ">"  => Greater
    case ">=" => GreaterEqual
  }

  def CreateExpression(op : String, left : Expression, right : Expression) : Expression = CreateExpression(str2op(op), left, right)
  def CreateExpression(op : Value, left : Expression, right : Expression) : Expression = op match {
    case Addition       => return new AdditionExpression(left, right)
    case Subtraction    => return new SubtractionExpression(left, right)
    case Multiplication => return new MultiplicationExpression(left, right)
    case Division       => return new DivisionExpression(left, right)
    case Modulo         => return new ModuloExpression(left, right)
    case Power          => return new PowerExpression(left, right)

    case EqEq           => return new EqEqExpression(left, right)
    case NeqNeq         => return new NeqNeqExpression(left, right)
    case AndAnd         => return new AndAndExpression(left, right)
    case OrOr           => return new OrOrExpression(left, right)
    case Lower          => return new LowerExpression(left, right)
    case Greater        => return new GreaterExpression(left, right)
    case LowerEqual     => return new LowerEqualExpression(left, right)
    case GreaterEqual   => return new GreaterEqualExpression(left, right)
  }
}

object UnaryOperators extends Enumeration {
  type UnaryOperators = Value
  val Positive = Value("")
  val Negative = Value("-")
  val Not = Value("!")
  val AddressOf = Value("&")

  exastencils.core.Duplicate.registerImmutable(this.getClass())
}

trait Access extends Expression
trait Number extends Expression {
  def value : AnyVal
}

case class NullExpression() extends Expression {
  def cpp : String = ""
}

case class ConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def cpp = expressions.map(e => e.cpp).mkString("")
  override def ~(exp : Expression) : ConcatenationExpression = {
    expressions += exp
    this
  }
}

case class SpacedConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def cpp = expressions.map(e => e.cpp).mkString(" ")
  override def ~~(exp : Expression) : SpacedConcatenationExpression = {
    expressions += exp
    this
  }
}

case class StringConstant(var value : String) extends Expression {
  override def cpp = value
}

case class IntegerConstant(var v : Long) extends Number {
  override def cpp = value.toString
  override def value = v
}

case class FloatConstant(var v : Double) extends Number {
  override def cpp = value.toString
  override def value = v
}

case class BooleanConstant(var value : Boolean) extends Expression {
  override def cpp = value.toString
}

case class VariableAccess(var name : String, var dType : Option[Datatype] = None) extends Access {
  override def cpp = name
}

case class ArrayAccess(var base : Expression, var index : Expression) extends Access {
  override def cpp = {
    index match {
      case ind : MultiIndex => base.cpp + ind.cpp
      case ind : Expression => base.cpp + '[' + ind.cpp + ']'
    }
  }
}

case class OffsetIndex(var minOffset : Int, var maxOffset : Int, var index : Expression, var offset : Expression) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = OffsetIndex\n"

  def expandSpecial : AdditionExpression = {
    index + offset
  }
}

case class MultiIndex(
  var index_0 : Expression = new NullExpression,
  var index_1 : Expression = new NullExpression,
  var index_2 : Expression = new NullExpression)
    extends Expression with Traversable[Expression] {
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
      + ")")
  }

  def apply(i : Int) : Expression = {
    i match {
      case 0 => index_0
      case 1 => index_1
      case 2 => index_2
    }
  }

  def +(that : MultiIndex) : MultiIndex = {
    return new MultiIndex(
      if (!this(0).isInstanceOf[NullExpression] && !that(0).isInstanceOf[NullExpression]) { this(0) + that(0) } else { new NullExpression },
      if (!this(1).isInstanceOf[NullExpression] && !that(1).isInstanceOf[NullExpression]) { this(1) + that(1) } else { new NullExpression },
      if (!this(2).isInstanceOf[NullExpression] && !that(2).isInstanceOf[NullExpression]) { this(2) + that(2) } else { new NullExpression })
  }

  override def foreach[U](f : Expression => U) : Unit = {
    val dim : Int = Knowledge.dimensionality
    var i : Int = 0
    do {
      f(this(i))
      i += 1
    } while (i < dim)
  }
}

object DefaultLoopMultiIndex {
  def apply() : MultiIndex = { new MultiIndex(dimToString(0), dimToString(1), dimToString(2)) }
}

case class UnresolvedFieldAccess(var fieldOwner : Expression, var fieldIdentifier : String, var level : Int, var slot : Expression, var index : MultiIndex) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = UnresolvedFieldAccess\n"

  def resolveField : Field = {
    StateManager.findFirst[FieldCollection]().get.getFieldByIdentifier(fieldIdentifier, level).get
  }

  def expand : FieldAccess = {
    new FieldAccess(fieldOwner, resolveField, slot, index)
  }
}

case class DirectFieldAccess(var fieldOwner : Expression, var field : Field, var slot : Expression, var index : MultiIndex) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = FieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldOwner, field, slot, Mapping.resolveMultiIdx(field.layout, index))
  }
}

case class FieldAccess(var fieldOwner : Expression, var field : Field, var slot : Expression, var index : MultiIndex) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = FieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldOwner, field, slot, Mapping.resolveMultiIdx(field.layout, new MultiIndex(index, field.referenceOffset, _ + _)))
  }
}

case class ExternalFieldAccess(var name : Expression, var field : ExternalField, var index : MultiIndex) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = ExternalFieldAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(name, Mapping.resolveMultiIdx(field.layout, index))
  }
}

case class LinearizedFieldAccess(var fieldOwner : Expression, var field : Field, var slot : Expression, var index : Expression) extends Expression {
  override def cpp : String = {
    s"${fieldOwner.cpp}${field.codeName.cpp}[${slot.cpp}][${index.cpp}]"
  }
}

case class UnresolvedStencilAccess(var stencilIdentifier : String, level : Int) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = UnresolvedStencilAccess\n"
}

case class MemberAccess(var base : Access, var varAcc : VariableAccess) extends Access {
  override def cpp = base.cpp + '.' + varAcc.cpp
}

case class DerefAccess(var base : Access) extends Access {
  override def cpp = "*(" + base.cpp + ')'
}

case class UnaryExpression(var operator : UnaryOperators.Value, var expression : Expression) extends Expression {
  override def cpp = { s"${operator.toString}(${expression.cpp})" }
}

case class AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("+")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("-")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("*")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("/")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class ModuloExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("%")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class PowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("pow(") // FIXME: check for integer constant => use pown
    left.cppsb(sb)
    sb.append(", ")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class EqEqExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("==")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class NeqNeqExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("!=")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class AndAndExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("&&")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class OrOrExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("||")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class LowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("<")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class GreaterExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append(">")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class LowerEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append("<=")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class GreaterEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("(")
    left.cppsb(sb)
    sb.append(">=")
    right.cppsb(sb)
    sb.append(")")
  }
}

case class FunctionCallExpression(var name : Expression, var arguments : ListBuffer[Expression /* FIXME: more specialization*/ ]) extends Expression {
  def this(name : Expression, argument : Expression) = this(name, ListBuffer(argument))

  override def cpp : String = {
    return (s"${name.cpp}(" + arguments.map(arg => arg.cpp).mkString(", ") + ")")
  }
}

case class MemberFunctionCallExpression(var objectName : Expression, var name : Expression, var arguments : ListBuffer[Expression /* FIXME: more specialization*/ ]) extends Expression {
  override def cpp : String = {
    return (s"${objectName.cpp}.${name.cpp}(" + arguments.map(arg => arg.cpp).mkString(", ") + ")")
  }
}

case class TernaryConditionExpression(var condition : Expression, var trueBody : Expression, var falseBody : Expression) extends Expression {
  def cpp : String = {
    (s"((${condition.cpp}) ? (${trueBody.cpp}) : (${falseBody.cpp}))")
  }
}

case class Reduction(var op : BinaryOperators.Value, var target : Expression) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = Reduction\n"

  def getOMPClause : Expression = {
    s"reduction(${BinaryOperators.op2str(op)}:" ~ target ~ ")"
  }
}