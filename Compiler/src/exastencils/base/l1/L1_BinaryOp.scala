package exastencils.base.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting.PpStream

/// supported operators

object L1_BinaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerConstant(this)

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

  def createExpression(op : String, left : L1_Expression, right : L1_Expression) : L1_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : L1_Expression, right : L1_Expression) : L1_Expression = op match {
    case Addition       => L1_Addition(left, right)
    case Subtraction    => L1_Subtraction(left, right)
    case Multiplication => L1_Multiplication(left, right)
    case Division       => L1_Division(left, right)
    case Power          => L1_Power(left, right)
    case Power_Alt      => L1_Power(left, right)
    case Modulo         => L1_Modulo(left, right)

    case ElementwiseAddition       => L1_ElementwiseAddition(left, right)
    case ElementwiseSubtraction    => L1_ElementwiseSubtraction(left, right)
    case ElementwiseMultiplication => L1_ElementwiseMultiplication(left, right)
    case ElementwiseDivision       => L1_ElementwiseDivision(left, right)
    case ElementwisePower          => L1_ElementwisePower(left, right)
    case ElementwiseModulo         => L1_ElementwiseModulo(left, right)

    case AndAnd | AndAndWritten => L1_AndAnd(left, right)
    case OrOr | OrOrWritten     => L1_OrOr(left, right)
    case Negation               => L1_Negation(left)
    case EqEq                   => L1_EqEq(left, right)
    case Neq                    => L1_Neq(left, right)
    case Lower                  => L1_Lower(left, right)
    case LowerEqual             => L1_LowerEqual(left, right)
    case Greater                => L1_Greater(left, right)
    case GreaterEqual           => L1_GreaterEqual(left, right)

    case Maximum => L1_Maximum(left, right)
    case Minimum => L1_Minimum(left, right)
  }

  def progress(op : Value) : L2_BinaryOperators.BinaryOperators = {
    // TODO: better implementation?
    L2_BinaryOperators.withName(op.toString)
  }
}

/// (scalar) arithmetic operations

object L1_Addition {
  def apply(varargs : L1_Expression*) = new L1_Addition(varargs.to[ListBuffer])
}

case class L1_Addition(var summands : ListBuffer[L1_Expression]) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket <<< (summands, " + ") << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Addition(summands.map(_.progress)))
}

case class L1_Subtraction(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " - " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Subtraction(left.progress, right.progress))
}

object L1_Multiplication {
  def apply(varargs : L1_Expression*) = new L1_Multiplication(varargs.to[ListBuffer])
}

case class L1_Multiplication(var factors : ListBuffer[L1_Expression]) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket <<< (factors, " * ") << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Multiplication(factors.map(_.progress)))
}

case class L1_Division(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " / " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Division(left.progress, right.progress))
}

case class L1_Modulo(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " % " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Modulo(left.progress, right.progress))
}

case class L1_Power(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " ** " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Power(left.progress, right.progress))
}

/// element-wise arithmetic operations

case class L1_ElementwiseAddition(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .+ " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_ElementwiseAddition(left.progress, right.progress))
}

case class L1_ElementwiseSubtraction(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .+ " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_ElementwiseSubtraction(left.progress, right.progress))
}

case class L1_ElementwiseMultiplication(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .* " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_ElementwiseMultiplication(left.progress, right.progress))
}

case class L1_ElementwiseDivision(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " ./ " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_ElementwiseDivision(left.progress, right.progress))
}

case class L1_ElementwiseModulo(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " .% " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_ElementwiseModulo(left.progress, right.progress))
}

case class L1_ElementwisePower(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << "dotpow ( " << left << ", " << right << beforeClosingBracket << ')'
  // FIXME: check for integer constant => use pown
  override def progress = ProgressLocation(L2_ElementwisePower(left.progress, right.progress))
}

/// logical comparison operations

case class L1_EqEq(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " == " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_EqEq(left.progress, right.progress))
}

case class L1_Neq(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " != " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Neq(left.progress, right.progress))
}

case class L1_Lower(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " < " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Lower(left.progress, right.progress))
}

case class L1_Greater(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " > " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_Greater(left.progress, right.progress))
}

case class L1_LowerEqual(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " <= " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_LowerEqual(left.progress, right.progress))
}

case class L1_GreaterEqual(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " >= " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_GreaterEqual(left.progress, right.progress))
}

/// (scalar) logical operations

case class L1_AndAnd(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " && " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_AndAnd(left.progress, right.progress))
}

case class L1_OrOr(var left : L1_Expression, var right : L1_Expression) extends L1_Expression {

  import L1_BinaryOperators._

  override def prettyprint(out : PpStream) = out << '(' << afterOpeningBracket << left << " || " << right << beforeClosingBracket << ')'
  override def progress = ProgressLocation(L2_OrOr(left.progress, right.progress))
}

/// min/max operations

object L1_Minimum {
  def apply(varargs : L1_Expression*) = new L1_Minimum(varargs.to[ListBuffer])
}

case class L1_Minimum(var args : ListBuffer[L1_Expression]) extends L1_Expression {

  override def prettyprint(out : PpStream) = out << "min ( " <<< (args, ", ") << " )"
  override def progress = ProgressLocation(L2_Minimum(args.map(_.progress)))
}

object L1_Maximum {
  def apply(varargs : L1_Expression*) = new L1_Maximum(varargs.to[ListBuffer])
}

case class L1_Maximum(var args : ListBuffer[L1_Expression]) extends L1_Expression {

  override def prettyprint(out : PpStream) = out << "max ( " <<< (args, ", ") << " )"
  override def progress = ProgressLocation(L2_Maximum(args.map(_.progress)))
}
