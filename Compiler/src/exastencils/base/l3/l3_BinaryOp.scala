package exastencils.base.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.prettyprinting.PpStream

/// supported operators

object L3_BinaryOperators extends Enumeration {
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

  def createExpression(op : String, left : L3_Expression, right : L3_Expression) : L3_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : L3_Expression, right : L3_Expression) : L3_Expression = op match {
    case Addition       => L3_AdditionExpression(left, right)
    case Subtraction    => L3_SubtractionExpression(left, right)
    case Multiplication => L3_MultiplicationExpression(left, right)
    case Division       => L3_DivisionExpression(left, right)
    case Power          => L3_PowerExpression(left, right)
    case Power_Alt      => L3_PowerExpression(left, right)
    case Modulo         => L3_ModuloExpression(left, right)

    case ElementwiseAddition       => L3_ElementwiseAdditionExpression(left, right)
    case ElementwiseSubtraction    => L3_ElementwiseSubtractionExpression(left, right)
    case ElementwiseMultiplication => L3_ElementwiseMultiplicationExpression(left, right)
    case ElementwiseDivision       => L3_ElementwiseDivisionExpression(left, right)
    case ElementwisePower          => L3_ElementwisePowerExpression(left, right)
    case ElementwiseModulo         => L3_ElementwiseModuloExpression(left, right)

    case AndAnd | AndAndWritten => L3_AndAndExpression(left, right)
    case OrOr | OrOrWritten     => L3_OrOrExpression(left, right)
    case Negation               => L3_NegationExpression(left)
    case EqEq                   => L3_EqEqExpression(left, right)
    case Neq                    => L3_NeqExpression(left, right)
    case Lower                  => L3_LowerExpression(left, right)
    case LowerEqual             => L3_LowerEqualExpression(left, right)
    case Greater                => L3_GreaterExpression(left, right)
    case GreaterEqual           => L3_GreaterEqualExpression(left, right)
  }
}

/// (scalar) arithmetic operations

object L3_AdditionExpression {
  def apply(varargs : L3_Expression*) = new L3_AdditionExpression(varargs.to[ListBuffer])
}

case class L3_AdditionExpression(var summands : ListBuffer[L3_Expression]) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
  override def progress = L4_AdditionExpression(summands.map(_.progress))
}

case class L3_SubtractionExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
  override def progress = L4_SubtractionExpression(left.progress, right.progress)
}

object L3_MultiplicationExpression {
  def apply(varargs : L3_Expression*) = new L3_MultiplicationExpression(varargs.to[ListBuffer])
}

case class L3_MultiplicationExpression(var factors : ListBuffer[L3_Expression]) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
  override def progress = L4_MultiplicationExpression(factors.map(_.progress))
}

case class L3_DivisionExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
  override def progress = L4_DivisionExpression(left.progress, right.progress)
}

case class L3_ModuloExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
  override def progress = L4_ModuloExpression(left.progress, right.progress)
}

case class L3_PowerExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "**" << right
  override def progress = L4_PowerExpression(left.progress, right.progress)
}

/// element-wise arithmetic operations

case class L3_ElementwiseAdditionExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = L4_ElementwiseAdditionExpression(left.progress, right.progress)
}

case class L3_ElementwiseSubtractionExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = L4_ElementwiseSubtractionExpression(left.progress, right.progress)
}

case class L3_ElementwiseMultiplicationExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
  override def progress = L4_ElementwiseMultiplicationExpression(left.progress, right.progress)
}

case class L3_ElementwiseDivisionExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
  override def progress = L4_ElementwiseDivisionExpression(left.progress, right.progress)
}

case class L3_ElementwiseModuloExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
  override def progress = L4_ElementwiseModuloExpression(left.progress, right.progress)
}

case class L3_ElementwisePowerExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')'
  // FIXME: check for integer constant => use pown
  override def progress = L4_ElementwisePowerExpression(left.progress, right.progress)
}

/// logical comparison operations

case class L3_EqEqExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
  override def progress = L4_EqEqExpression(left.progress, right.progress)
}

case class L3_NeqExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
  override def progress = L4_NeqExpression(left.progress, right.progress)
}

case class L3_LowerExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
  override def progress = L4_LowerExpression(left.progress, right.progress)
}

case class L3_GreaterExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
  override def progress = L4_GreaterExpression(left.progress, right.progress)
}

case class L3_LowerEqualExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
  override def progress = L4_LowerEqualExpression(left.progress, right.progress)
}

case class L3_GreaterEqualExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
  override def progress = L4_GreaterEqualExpression(left.progress, right.progress)
}

/// (scalar) logical operations

case class L3_AndAndExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
  override def progress = L4_AndAndExpression(left.progress, right.progress)
}

case class L3_OrOrExpression(var left : L3_Expression, var right : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
  override def progress = L4_OrOrExpression(left.progress, right.progress)
}
