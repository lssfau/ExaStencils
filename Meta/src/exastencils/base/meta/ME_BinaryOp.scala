package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_BinaryOp extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_BinaryOp.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.base.|NEXT_LC|._
import exastencils.prettyprinting.PpStream

/// supported operators

object |LAYER_UC|_BinaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  type BinaryOperators = Value
  val Addition = Value("+")
  val Subtraction = Value("-")
  val Multiplication = Value("*")
  val Division = Value("/")
  val Power = Value("**")
  val Power_Alt = Value("^")
  val Modulo = Value("%")

  val ElementwiseAddition = Value(".+")
  val ElementwiseSubtraction = Value(".-")
  val ElementwiseMultiplication = Value(".*")
  val ElementwiseDivision = Value("./")
  val ElementwisePower = Value(".**")
  val ElementwiseModulo = Value(".%")

  val AndAnd = Value("&&")
  val AndAndWritten = Value("and")
  val OrOr = Value("||")
  val OrOrWritten = Value("or")
  val Negation = Value("!")
  val EqEq = Value("==")
  val Neq = Value("!=")
  val Lower = Value("<")
  val LowerEqual = Value("<=")
  val Greater = Value(">")
  val GreaterEqual = Value(">=")

  val Maximum = Value("max")
  val Minimum = Value("min")

  //  Conversions for Enumeration:
  // BinaryOperators -> String:  op.toString()
  // String -> BinaryOperators:  BinaryOperators.withName(op)

  def createExpression(op : String, left : |LAYER_UC|_Expression, right : |LAYER_UC|_Expression) : |LAYER_UC|_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : |LAYER_UC|_Expression, right : |LAYER_UC|_Expression) : |LAYER_UC|_Expression = op match {
    case Addition       => |LAYER_UC|_Addition(left, right)
    case Subtraction    => |LAYER_UC|_Subtraction(left, right)
    case Multiplication => |LAYER_UC|_Multiplication(left, right)
    case Division       => |LAYER_UC|_Division(left, right)
    case Power          => |LAYER_UC|_Power(left, right)
    case Power_Alt      => |LAYER_UC|_Power(left, right)
    case Modulo         => |LAYER_UC|_Modulo(left, right)

    case ElementwiseAddition       => |LAYER_UC|_ElementwiseAddition(left, right)
    case ElementwiseSubtraction    => |LAYER_UC|_ElementwiseSubtraction(left, right)
    case ElementwiseMultiplication => |LAYER_UC|_ElementwiseMultiplication(left, right)
    case ElementwiseDivision       => |LAYER_UC|_ElementwiseDivision(left, right)
    case ElementwisePower          => |LAYER_UC|_ElementwisePower(left, right)
    case ElementwiseModulo         => |LAYER_UC|_ElementwiseModulo(left, right)

    case AndAnd | AndAndWritten => |LAYER_UC|_AndAnd(left, right)
    case OrOr | OrOrWritten     => |LAYER_UC|_OrOr(left, right)
    case Negation               => |LAYER_UC|_Negation(left)
    case EqEq                   => |LAYER_UC|_EqEq(left, right)
    case Neq                    => |LAYER_UC|_Neq(left, right)
    case Lower                  => |LAYER_UC|_Lower(left, right)
    case LowerEqual             => |LAYER_UC|_LowerEqual(left, right)
    case Greater                => |LAYER_UC|_Greater(left, right)
    case GreaterEqual           => |LAYER_UC|_GreaterEqual(left, right)

    case Maximum => |LAYER_UC|_Maximum(left, right)
    case Minimum => |LAYER_UC|_Minimum(left, right)
  }

  def progress(op : Value) : |NEXT_UC|_BinaryOperators.BinaryOperators = {
    // TODO: better implementation?
    |NEXT_UC|_BinaryOperators.withName(op.toString)
  }
}

/// (scalar) arithmetic operations

object |LAYER_UC|_Addition {
  def apply(varargs : |LAYER_UC|_Expression*) = new |LAYER_UC|_Addition(varargs.to[ListBuffer])
}

case class |LAYER_UC|_Addition(var summands : ListBuffer[|LAYER_UC|_Expression]) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
  override def progress = |NEXT_UC|_Addition(summands.map(_.progress))
}

case class |LAYER_UC|_Subtraction(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
  override def progress = |NEXT_UC|_Subtraction(left.progress, right.progress)
}

object |LAYER_UC|_Multiplication {
  def apply(varargs : |LAYER_UC|_Expression*) = new |LAYER_UC|_Multiplication(varargs.to[ListBuffer])
}

case class |LAYER_UC|_Multiplication(var factors : ListBuffer[|LAYER_UC|_Expression]) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
  override def progress = |NEXT_UC|_Multiplication(factors.map(_.progress))
}

case class |LAYER_UC|_Division(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
  override def progress = |NEXT_UC|_Division(left.progress, right.progress)
}

case class |LAYER_UC|_Modulo(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
  override def progress = |NEXT_UC|_Modulo(left.progress, right.progress)
}

case class |LAYER_UC|_Power(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "**" << right
  override def progress = |NEXT_UC|_Power(left.progress, right.progress)
}

/// element-wise arithmetic operations

case class |LAYER_UC|_ElementwiseAddition(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = |NEXT_UC|_ElementwiseAddition(left.progress, right.progress)
}

case class |LAYER_UC|_ElementwiseSubtraction(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = |NEXT_UC|_ElementwiseSubtraction(left.progress, right.progress)
}

case class |LAYER_UC|_ElementwiseMultiplication(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
  override def progress = |NEXT_UC|_ElementwiseMultiplication(left.progress, right.progress)
}

case class |LAYER_UC|_ElementwiseDivision(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
  override def progress = |NEXT_UC|_ElementwiseDivision(left.progress, right.progress)
}

case class |LAYER_UC|_ElementwiseModulo(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
  override def progress = |NEXT_UC|_ElementwiseModulo(left.progress, right.progress)
}

case class |LAYER_UC|_ElementwisePower(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')'
  // FIXME: check for integer constant => use pown
  override def progress = |NEXT_UC|_ElementwisePower(left.progress, right.progress)
}

/// logical comparison operations

case class |LAYER_UC|_EqEq(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
  override def progress = |NEXT_UC|_EqEq(left.progress, right.progress)
}

case class |LAYER_UC|_Neq(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
  override def progress = |NEXT_UC|_Neq(left.progress, right.progress)
}

case class |LAYER_UC|_Lower(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
  override def progress = |NEXT_UC|_Lower(left.progress, right.progress)
}

case class |LAYER_UC|_Greater(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
  override def progress = |NEXT_UC|_Greater(left.progress, right.progress)
}

case class |LAYER_UC|_LowerEqual(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
  override def progress = |NEXT_UC|_LowerEqual(left.progress, right.progress)
}

case class |LAYER_UC|_GreaterEqual(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
  override def progress = |NEXT_UC|_GreaterEqual(left.progress, right.progress)
}

/// (scalar) logical operations

case class |LAYER_UC|_AndAnd(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
  override def progress = |NEXT_UC|_AndAnd(left.progress, right.progress)
}

case class |LAYER_UC|_OrOr(var left : |LAYER_UC|_Expression, var right : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
  override def progress = |NEXT_UC|_OrOr(left.progress, right.progress)
}

/// min/max operations

object |LAYER_UC|_Minimum {
  def apply(varargs : |LAYER_UC|_Expression*) = new |LAYER_UC|_Minimum(varargs.to[ListBuffer])
}

case class |LAYER_UC|_Minimum(var args : ListBuffer[|LAYER_UC|_Expression]) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) = out << "min (" <<< (args, ", ") << ')'
  override def progress = |NEXT_UC|_Minimum(args.map(_.progress))
}

object |LAYER_UC|_Maximum {
  def apply(varargs : |LAYER_UC|_Expression*) = new |LAYER_UC|_Maximum(varargs.to[ListBuffer])
}

case class |LAYER_UC|_Maximum(var args : ListBuffer[|LAYER_UC|_Expression]) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) = out << "max (" <<< (args, ", ") << ')'
  override def progress = |NEXT_UC|_Maximum(args.map(_.progress))
}
"""
  }
}
