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
    case Addition       => L2_AdditionExpression(left, right)
    case Subtraction    => L2_SubtractionExpression(left, right)
    case Multiplication => L2_MultiplicationExpression(left, right)
    case Division       => L2_DivisionExpression(left, right)
    case Power          => L2_PowerExpression(left, right)
    case Power_Alt      => L2_PowerExpression(left, right)
    case Modulo         => L2_ModuloExpression(left, right)

    case ElementwiseAddition       => L2_ElementwiseAdditionExpression(left, right)
    case ElementwiseSubtraction    => L2_ElementwiseSubtractionExpression(left, right)
    case ElementwiseMultiplication => L2_ElementwiseMultiplicationExpression(left, right)
    case ElementwiseDivision       => L2_ElementwiseDivisionExpression(left, right)
    case ElementwisePower          => L2_ElementwisePowerExpression(left, right)
    case ElementwiseModulo         => L2_ElementwiseModuloExpression(left, right)

    case AndAnd | AndAndWritten => L2_AndAndExpression(left, right)
    case OrOr | OrOrWritten     => L2_OrOrExpression(left, right)
    case Negation               => L2_NegationExpression(left)
    case EqEq                   => L2_EqEqExpression(left, right)
    case Neq                    => L2_NeqExpression(left, right)
    case Lower                  => L2_LowerExpression(left, right)
    case LowerEqual             => L2_LowerEqualExpression(left, right)
    case Greater                => L2_GreaterExpression(left, right)
    case GreaterEqual           => L2_GreaterEqualExpression(left, right)
  }
}

/// (scalar) arithmetic operations

object L2_AdditionExpression {
  def apply(varargs : L2_Expression*) = new L2_AdditionExpression(varargs.to[ListBuffer])
}

case class L2_AdditionExpression(var summands : ListBuffer[L2_Expression]) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
  override def progress = L3_AdditionExpression(summands.map(_.progress))
}

case class L2_SubtractionExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
  override def progress = L3_SubtractionExpression(left.progress, right.progress)
}

object L2_MultiplicationExpression {
  def apply(varargs : L2_Expression*) = new L2_MultiplicationExpression(varargs.to[ListBuffer])
}

case class L2_MultiplicationExpression(var factors : ListBuffer[L2_Expression]) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
  override def progress = L3_MultiplicationExpression(factors.map(_.progress))
}

case class L2_DivisionExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
  override def progress = L3_DivisionExpression(left.progress, right.progress)
}

case class L2_ModuloExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
  override def progress = L3_ModuloExpression(left.progress, right.progress)
}

case class L2_PowerExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "**" << right
  override def progress = L3_PowerExpression(left.progress, right.progress)
}

/// element-wise arithmetic operations

case class L2_ElementwiseAdditionExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = L3_ElementwiseAdditionExpression(left.progress, right.progress)
}

case class L2_ElementwiseSubtractionExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = L3_ElementwiseSubtractionExpression(left.progress, right.progress)
}

case class L2_ElementwiseMultiplicationExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
  override def progress = L3_ElementwiseMultiplicationExpression(left.progress, right.progress)
}

case class L2_ElementwiseDivisionExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
  override def progress = L3_ElementwiseDivisionExpression(left.progress, right.progress)
}

case class L2_ElementwiseModuloExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
  override def progress = L3_ElementwiseModuloExpression(left.progress, right.progress)
}

case class L2_ElementwisePowerExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')'
  // FIXME: check for integer constant => use pown
  override def progress = L3_ElementwisePowerExpression(left.progress, right.progress)
}

/// logical comparison operations

case class L2_EqEqExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
  override def progress = L3_EqEqExpression(left.progress, right.progress)
}

case class L2_NeqExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
  override def progress = L3_NeqExpression(left.progress, right.progress)
}

case class L2_LowerExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
  override def progress = L3_LowerExpression(left.progress, right.progress)
}

case class L2_GreaterExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
  override def progress = L3_GreaterExpression(left.progress, right.progress)
}

case class L2_LowerEqualExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
  override def progress = L3_LowerEqualExpression(left.progress, right.progress)
}

case class L2_GreaterEqualExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
  override def progress = L3_GreaterEqualExpression(left.progress, right.progress)
}

/// (scalar) logical operations

case class L2_AndAndExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
  override def progress = L3_AndAndExpression(left.progress, right.progress)
}

case class L2_OrOrExpression(var left : L2_Expression, var right : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
  override def progress = L3_OrOrExpression(left.progress, right.progress)
}
