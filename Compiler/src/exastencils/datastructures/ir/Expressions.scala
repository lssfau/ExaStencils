package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

import exastencils.core.Duplicate
import exastencils.datastructures.Node
import exastencils.datastructures.ir.ImplicitConversions.StringToStringLiteral
import exastencils.knowledge.ExternalField
import exastencils.knowledge.FieldSelection
import exastencils.knowledge.Mapping
import exastencils.knowledge.Stencil
import exastencils.knowledge.StencilFieldSelection

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
  val Addition, Subtraction, Multiplication, Division, Power, Modulo, AndAnd, OrOr, EqEq, NeqNeq, Lower, LowerEqual, Greater, GreaterEqual, BitwiseAnd = Value

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
    case BitwiseAnd     => "&"
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
    case "&"  => BitwiseAnd
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

    case BitwiseAnd     => return new BitwiseAndExpression(left, right)
  }
}

object UnaryOperators extends Enumeration {
  type UnaryOperators = Value
  val Positive = Value("")
  val Negative = Value("-")
  val Not = Value("!")
  val AddressOf = Value("&")
  val BitwiseNegation = Value("~")

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
  override def cpp = String.format(java.util.Locale.US, "%e", Double.box(value)) // ensure the compiler can parse the string
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
  var index_2 : Expression = new NullExpression,
  var index_3 : Expression = new NullExpression)
    extends Expression with Traversable[Expression] {
  def this(indices : Array[Expression]) = this(
    if (indices.length > 0) indices(0) else new NullExpression,
    if (indices.length > 1) indices(1) else new NullExpression,
    if (indices.length > 2) indices(2) else new NullExpression,
    if (indices.length > 3) indices(3) else new NullExpression)
  def this(indices : Array[Int]) = this(
    (if (indices.length > 0) IntegerConstant(indices(0)) else new NullExpression) : Expression,
    (if (indices.length > 1) IntegerConstant(indices(1)) else new NullExpression) : Expression,
    (if (indices.length > 2) IntegerConstant(indices(2)) else new NullExpression) : Expression,
    (if (indices.length > 3) IntegerConstant(indices(3)) else new NullExpression) : Expression)
  def this(names : String*) = this(
    (if (names.size > 0) VariableAccess(names(0), Some(IntegerDatatype())) else new NullExpression) : Expression,
    (if (names.size > 1) VariableAccess(names(1), Some(IntegerDatatype())) else new NullExpression) : Expression,
    (if (names.size > 2) VariableAccess(names(2), Some(IntegerDatatype())) else new NullExpression) : Expression,
    (if (names.size > 3) VariableAccess(names(3), Some(IntegerDatatype())) else new NullExpression) : Expression)
  def this(left : MultiIndex, right : MultiIndex, f : (Expression, Expression) => Expression) = this(
    if (!left(0).isInstanceOf[NullExpression] && !right(0).isInstanceOf[NullExpression]) { Duplicate(f(left(0), right(0))) } else { new NullExpression },
    if (!left(1).isInstanceOf[NullExpression] && !right(1).isInstanceOf[NullExpression]) { Duplicate(f(left(1), right(1))) } else { new NullExpression },
    if (!left(2).isInstanceOf[NullExpression] && !right(2).isInstanceOf[NullExpression]) { Duplicate(f(left(2), right(2))) } else { new NullExpression },
    if (!left(3).isInstanceOf[NullExpression] && !right(3).isInstanceOf[NullExpression]) { Duplicate(f(left(3), right(3))) } else { new NullExpression })

  override def cpp = {
    ('['
      + index_0.cpp
      + (if (!index_1.isInstanceOf[NullExpression]) s", ${index_1.cpp}" else "")
      + (if (!index_2.isInstanceOf[NullExpression]) s", ${index_2.cpp}" else "")
      + (if (!index_3.isInstanceOf[NullExpression]) s", ${index_3.cpp}" else "")
      + ']')
  }

  def apply(i : Int) : Expression = {
    i match {
      case 0 => index_0
      case 1 => index_1
      case 2 => index_2
      case 3 => index_3
    }
  }

  def update(i : Int, up : Expression) : Unit = {
    i match {
      case 0 => index_0 = up
      case 1 => index_1 = up
      case 2 => index_2 = up
      case 3 => index_3 = up
    }
  }

  def +(that : MultiIndex) : MultiIndex = new MultiIndex(this, that, _ + _)

  override def foreach[U](f : Expression => U) : Unit = {
    // TODO: check functionality for vector fields
    var i : Int = 0
    do {
      f(this(i))
      i += 1
    } while (i < 4 && this(i) != NullExpression())
  }
}

case class DirectFieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = FieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.layout, index))
  }
}

case class FieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = FieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.layout, new MultiIndex(index, fieldSelection.referenceOffset, _ + _)))
  }
}

case class ExternalFieldAccess(var name : Expression, var field : ExternalField, var index : MultiIndex) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = ExternalFieldAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(name, Mapping.resolveMultiIdx(field.layout, index))
  }
}

case class LinearizedFieldAccess(var fieldSelection : FieldSelection, var index : Expression) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ExternalFieldAccess\n"

  override def expand : Expression = {
    new ArrayAccess(new iv.FieldData(fieldSelection.field, fieldSelection.slot, fieldSelection.fragIdx), index.cpp)
  }
}

case class StencilAccess(var stencil : Stencil) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = StencilAccess\n"
}

case class StencilFieldAccess(var stencilFieldSelection : StencilFieldSelection, var index : MultiIndex) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = StencilFieldAccess\n"
}

case class MemberAccess(var base : Access, var varAcc : VariableAccess) extends Access {
  override def cpp = base.cpp + '.' + varAcc.cpp
}

case class DerefAccess(var base : Access) extends Access {
  override def cpp = "(*" + base.cpp + ')'
}

case class UnaryExpression(var operator : UnaryOperators.Value, var expression : Expression) extends Expression {
  override def cpp = { '(' + operator.toString() + expression.cpp + ')' }
}

case class AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('+')
    right.cppsb(sb)
    sb.append(')')
  }
}

case class SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('-')
    right.cppsb(sb)
    sb.append(')')
  }
}

case class MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('*')
    right.cppsb(sb)
    sb.append(')')
  }
}

case class DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('/')
    right.cppsb(sb)
    sb.append(')')
  }
}

case class ModuloExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('%')
    right.cppsb(sb)
    sb.append(')')
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
    sb.append(')')
  }
}

case class EqEqExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append("==")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class NeqNeqExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append("!=")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class AndAndExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append("&&")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class OrOrExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append("||")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class LowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('<')
    right.cppsb(sb)
    sb.append(')')
  }
}

case class GreaterExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('>')
    right.cppsb(sb)
    sb.append(')')
  }
}

case class LowerEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append("<=")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class GreaterEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append(">=")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class BitwiseAndExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append('(')
    left.cppsb(sb)
    sb.append('&')
    right.cppsb(sb)
    sb.append(')')
  }
}

case class FunctionCallExpression(var name : Expression, var arguments : ListBuffer[Expression]) extends Expression {
  def this(name : Expression, argument : Expression) = this(name, ListBuffer(argument))

  override def cpp : String = {
    return (s"${name.cpp}(" + arguments.map(arg => arg.cpp).mkString(", ") + ')')
  }
}

case class InitializerList(var arguments : ListBuffer[Expression]) extends Expression {
  def this(argument : Expression) = this(ListBuffer(argument))

  override def cpp : String = {
    val sb = new StringBuilder()
    cppsb(sb)
    return sb.toString()
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("{ ")
    for (arg <- arguments) {
      arg.cppsb(sb)
      sb.append(", ")
    }
    val l : Int = sb.length
    sb.replace(l - 2, l, " }")
  }
}

case class MemberFunctionCallExpression(var objectName : Expression, var name : Expression, var arguments : ListBuffer[Expression]) extends Expression {
  override def cpp : String = {
    return (s"${objectName.cpp}.${name.cpp}(" + arguments.map(arg => arg.cpp).mkString(", ") + ')')
  }
}

case class TernaryConditionExpression(var condition : Expression, var trueBody : Expression, var falseBody : Expression) extends Expression {
  override def cpp : String = {
    (s"(${condition.cpp} ? ${trueBody.cpp} : ${falseBody.cpp})")
  }
}

case class Reduction(var op : BinaryOperators.Value, var target : Expression) extends Expression {
  override def cpp : String = "NOT VALID ; CLASS = Reduction\n"

  def getOMPClause : Expression = {
    s"reduction(${BinaryOperators.op2str(op)}:" ~ target ~ ")"
  }
}

// TODO: determine instruction according to target architecture

case class SIMD_LoadExpression(var mem : Expression, var aligned : Boolean) extends Expression {
  override def cpp : String = {
    val sb = new StringBuilder()
    cppsb(sb)
    return sb.toString()
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    if (aligned)
      sb.append("_mm256_load_pd(")
    else
      sb.append("_mm256_loadu_pd(")
    mem.cppsb(sb)
    sb.append(')')
  }
}

case class SIMD_AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("_mm256_add_pd(")
    left.cppsb(sb)
    sb.append(", ")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class SIMD_SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("_mm256_sub_pd(")
    left.cppsb(sb)
    sb.append(", ")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class SIMD_MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("_mm256_mul_pd(")
    left.cppsb(sb)
    sb.append(", ")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class SIMD_MultiplyAddExpression(var factor1 : Expression, var factor2 : Expression, var summand : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("_mm256_add_pd(")
    sb.append("_mm256_mul_pd(")
    factor1.cppsb(sb)
    sb.append(", ")
    factor2.cppsb(sb)
    sb.append("), ")
    summand.cppsb(sb)
    sb.append(')')
  }
}

case class SIMD_MultiplySubExpression(var factor1 : Expression, var factor2 : Expression, var summand : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("_mm256_sub_pd(")
    sb.append("_mm256_mul_pd(")
    factor1.cppsb(sb)
    sb.append(", ")
    factor2.cppsb(sb)
    sb.append("), ")
    summand.cppsb(sb)
    sb.append(')')
  }
}

case class SIMD_DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp : String = {
    var sb = new StringBuilder
    cppsb(sb)
    return sb.toString
  }

  override def cppsb(sb : StringBuilder) : Unit = {
    sb.append("_mm256_div_pd(")
    left.cppsb(sb)
    sb.append(", ")
    right.cppsb(sb)
    sb.append(')')
  }
}

case class SIMD_FloatConstantExpression(var value : Double) extends Expression {
  // ensure the compiler can parse the string
  override def cpp = String.format(java.util.Locale.US, "_mm256_set1_pd(%e)", Double.box(value))
}
