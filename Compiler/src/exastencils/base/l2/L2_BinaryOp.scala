package exastencils.base.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.prettyprinting.PpStream

/// supported operators

object L2_BinaryOperators extends Enumeration {
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

  //  Conversions for Enumeration:
  // BinaryOperators -> String:  op.toString()
  // String -> BinaryOperators:  BinaryOperators.withName(op)

  def createExpression(op : String, left : L2_Expression, right : L2_Expression) : L2_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : L2_Expression, right : L2_Expression) : L2_Expression = op match {
    case Addition       => L2_Addition(left, right)
    case Subtraction    => L2_Subtraction(left, right)
    case Multiplication => L2_Multiplication(left, right)
    case Division       => L2_Division(left, right)
    case Power          => L2_Power(left, right)
    case Power_Alt      => L2_Power(left, right)
    case Modulo         => L2_Modulo(left, right)

    case ElementwiseAddition       => L2_ElementwiseAddition(left, right)
    case ElementwiseSubtraction    => L2_ElementwiseSubtraction(left, right)
    case ElementwiseMultiplication => L2_ElementwiseMultiplication(left, right)
    case ElementwiseDivision       => L2_ElementwiseDivision(left, right)
    case ElementwisePower          => L2_ElementwisePower(left, right)
    case ElementwiseModulo         => L2_ElementwiseModulo(left, right)

    case AndAnd | AndAndWritten => L2_AndAnd(left, right)
    case OrOr | OrOrWritten     => L2_OrOr(left, right)
    case Negation               => L2_Negation(left)
    case EqEq                   => L2_EqEq(left, right)
    case Neq                    => L2_Neq(left, right)
    case Lower                  => L2_Lower(left, right)
    case LowerEqual             => L2_LowerEqual(left, right)
    case Greater                => L2_Greater(left, right)
    case GreaterEqual           => L2_GreaterEqual(left, right)
  }
}

/// (scalar) arithmetic operations

object L2_Addition {
  def apply(varargs : L2_Expression*) = new L2_Addition(varargs.to[ListBuffer])
}

case class L2_Addition(var summands : ListBuffer[L2_Expression]) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
  override def progress = L3_Addition(summands.map(_.progress))
}

case class L2_Subtraction(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
  override def progress = L3_Subtraction(left.progress, right.progress)
}

object L2_Multiplication {
  def apply(varargs : L2_Expression*) = new L2_Multiplication(varargs.to[ListBuffer])
}

case class L2_Multiplication(var factors : ListBuffer[L2_Expression]) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
  override def progress = L3_Multiplication(factors.map(_.progress))
}

case class L2_Division(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
  override def progress = L3_Division(left.progress, right.progress)
}

case class L2_Modulo(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
  override def progress = L3_Modulo(left.progress, right.progress)
}

case class L2_Power(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "**" << right
  override def progress = L3_Power(left.progress, right.progress)
}

/// element-wise arithmetic operations

case class L2_ElementwiseAddition(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = L3_ElementwiseAddition(left.progress, right.progress)
}

case class L2_ElementwiseSubtraction(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = L3_ElementwiseSubtraction(left.progress, right.progress)
}

case class L2_ElementwiseMultiplication(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
  override def progress = L3_ElementwiseMultiplication(left.progress, right.progress)
}

case class L2_ElementwiseDivision(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
  override def progress = L3_ElementwiseDivision(left.progress, right.progress)
}

case class L2_ElementwiseModulo(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
  override def progress = L3_ElementwiseModulo(left.progress, right.progress)
}

case class L2_ElementwisePower(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')'
  // FIXME: check for integer constant => use pown
  override def progress = L3_ElementwisePower(left.progress, right.progress)
}

/// logical comparison operations

case class L2_EqEq(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
  override def progress = L3_EqEq(left.progress, right.progress)
}

case class L2_Neq(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
  override def progress = L3_Neq(left.progress, right.progress)
}

case class L2_Lower(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
  override def progress = L3_Lower(left.progress, right.progress)
}

case class L2_Greater(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
  override def progress = L3_Greater(left.progress, right.progress)
}

case class L2_LowerEqual(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
  override def progress = L3_LowerEqual(left.progress, right.progress)
}

case class L2_GreaterEqual(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
  override def progress = L3_GreaterEqual(left.progress, right.progress)
}

/// (scalar) logical operations

case class L2_AndAnd(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
  override def progress = L3_AndAnd(left.progress, right.progress)
}

case class L2_OrOr(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
  override def progress = L3_OrOr(left.progress, right.progress)
}
