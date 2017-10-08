package exastencils.base.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.prettyprinting.PpStream

/// supported operators

object L3_BinaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  val afterOpeningBracket = " "
  val beforeClosingBracket = " "

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

  def createExpression(op : String, left : L3_Expression, right : L3_Expression) : L3_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : L3_Expression, right : L3_Expression) : L3_Expression = op match {
    case Addition       => L3_Addition(left, right)
    case Subtraction    => L3_Subtraction(left, right)
    case Multiplication => L3_Multiplication(left, right)
    case Division       => L3_Division(left, right)
    case Power          => L3_Power(left, right)
    case Power_Alt      => L3_Power(left, right)
    case Modulo         => L3_Modulo(left, right)

    case ElementwiseAddition       => L3_ElementwiseAddition(left, right)
    case ElementwiseSubtraction    => L3_ElementwiseSubtraction(left, right)
    case ElementwiseMultiplication => L3_ElementwiseMultiplication(left, right)
    case ElementwiseDivision       => L3_ElementwiseDivision(left, right)
    case ElementwisePower          => L3_ElementwisePower(left, right)
    case ElementwiseModulo         => L3_ElementwiseModulo(left, right)

    case AndAnd | AndAndWritten => L3_AndAnd(left, right)
    case OrOr | OrOrWritten     => L3_OrOr(left, right)
    case Negation               => L3_Negation(left)
    case EqEq                   => L3_EqEq(left, right)
    case Neq                    => L3_Neq(left, right)
    case Lower                  => L3_Lower(left, right)
    case LowerEqual             => L3_LowerEqual(left, right)
    case Greater                => L3_Greater(left, right)
    case GreaterEqual           => L3_GreaterEqual(left, right)

    case Maximum => L3_Maximum(left, right)
    case Minimum => L3_Minimum(left, right)
  }

  def progress(op : Value) : L4_BinaryOperators.BinaryOperators = {
    // TODO: better implementation?
    L4_BinaryOperators.withName(op.toString)
  }
}

/// (scalar) arithmetic operations

object L3_Addition {
  def apply(varargs : L3_Expression*) = new L3_Addition(varargs.to[ListBuffer])
}

case class L3_Addition(var summands : ListBuffer[L3_Expression]) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket <<< (summands, " + ") << beforeClosingBracket << ')'
  override def progress = L4_Addition(summands.map(_.progress))
}

case class L3_Subtraction(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " - " << right << beforeClosingBracket << ')'
  override def progress = L4_Subtraction(left.progress, right.progress)
}

object L3_Multiplication {
  def apply(varargs : L3_Expression*) = new L3_Multiplication(varargs.to[ListBuffer])
}

case class L3_Multiplication(var factors : ListBuffer[L3_Expression]) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket <<< (factors, " * ") << beforeClosingBracket << ')'
  override def progress = L4_Multiplication(factors.map(_.progress))
}

case class L3_Division(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " / " << right << beforeClosingBracket << ')'
  override def progress = L4_Division(left.progress, right.progress)
}

case class L3_Modulo(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " % " << right << beforeClosingBracket << ')'
  override def progress = L4_Modulo(left.progress, right.progress)
}

case class L3_Power(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " ** " << right << beforeClosingBracket << ')'
  override def progress = L4_Power(left.progress, right.progress)
}

/// element-wise arithmetic operations

case class L3_ElementwiseAddition(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .+ " << right << beforeClosingBracket << ')'
  override def progress = ??? // L4_ElementwiseAddition(left.progress, right.progress)
}

case class L3_ElementwiseSubtraction(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .+ " << right << beforeClosingBracket << ')'
  override def progress = ??? // L4_ElementwiseSubtraction(left.progress, right.progress)
}

case class L3_ElementwiseMultiplication(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .* " << right << beforeClosingBracket << ')'
  override def progress = L4_ElementwiseMultiplication(left.progress, right.progress)
}

case class L3_ElementwiseDivision(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " ./ " << right << beforeClosingBracket << ')'
  override def progress = L4_ElementwiseDivision(left.progress, right.progress)
}

case class L3_ElementwiseModulo(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .% " << right << beforeClosingBracket << ')'
  override def progress = L4_ElementwiseModulo(left.progress, right.progress)
}

case class L3_ElementwisePower(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << "dotpow ( " << left << ", " << right << beforeClosingBracket << ')'
  // FIXME: check for integer constant => use pown
  override def progress = L4_ElementwisePower(left.progress, right.progress)
}

/// logical comparison operations

case class L3_EqEq(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " == " << right << beforeClosingBracket << ')'
  override def progress = L4_EqEq(left.progress, right.progress)
}

case class L3_Neq(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " != " << right << beforeClosingBracket << ')'
  override def progress = L4_Neq(left.progress, right.progress)
}

case class L3_Lower(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " < " << right << beforeClosingBracket << ')'
  override def progress = L4_Lower(left.progress, right.progress)
}

case class L3_Greater(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " > " << right << beforeClosingBracket << ')'
  override def progress = L4_Greater(left.progress, right.progress)
}

case class L3_LowerEqual(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " <= " << right << beforeClosingBracket << ')'
  override def progress = L4_LowerEqual(left.progress, right.progress)
}

case class L3_GreaterEqual(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " >= " << right << beforeClosingBracket << ')'
  override def progress = L4_GreaterEqual(left.progress, right.progress)
}

/// (scalar) logical operations

case class L3_AndAnd(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " && " << right << beforeClosingBracket << ')'
  override def progress = L4_AndAnd(left.progress, right.progress)
}

case class L3_OrOr(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  import L3_BinaryOperators._
  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " || " << right << beforeClosingBracket << ')'
  override def progress = L4_OrOr(left.progress, right.progress)
}

/// min/max operations

object L3_Minimum {
  def apply(varargs : L3_Expression*) = new L3_Minimum(varargs.to[ListBuffer])
}

case class L3_Minimum(var args : ListBuffer[L3_Expression]) extends L3_Expression {
  override def prettyprint(out : PpStream) = out << "min ( " <<< (args, ", ") << " )"
  override def progress = L4_Minimum(args.map(_.progress))
}

object L3_Maximum {
  def apply(varargs : L3_Expression*) = new L3_Maximum(varargs.to[ListBuffer])
}

case class L3_Maximum(var args : ListBuffer[L3_Expression]) extends L3_Expression {
  override def prettyprint(out : PpStream) = out << "max ( " <<< (args, ", ") << " )"
  override def progress = L4_Maximum(args.map(_.progress))
}
