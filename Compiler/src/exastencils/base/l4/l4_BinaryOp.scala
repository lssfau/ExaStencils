package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// supported operators

object L4_BinaryOperators extends Enumeration {
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

  def createExpression(op : String, left : L4_Expression, right : L4_Expression) : L4_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : L4_Expression, right : L4_Expression) : L4_Expression = op match {
    case Addition       => L4_AdditionExpression(left, right)
    case Subtraction    => L4_SubtractionExpression(left, right)
    case Multiplication => L4_MultiplicationExpression(left, right)
    case Division       => L4_DivisionExpression(left, right)
    case Power          => L4_PowerExpression(left, right)
    case Power_Alt      => L4_PowerExpression(left, right)
    case Modulo         => L4_ModuloExpression(left, right)

    case ElementwiseAddition       => L4_ElementwiseAdditionExpression(left, right)
    case ElementwiseSubtraction    => L4_ElementwiseSubtractionExpression(left, right)
    case ElementwiseMultiplication => L4_ElementwiseMultiplicationExpression(left, right)
    case ElementwiseDivision       => L4_ElementwiseDivisionExpression(left, right)
    case ElementwisePower          => L4_ElementwisePowerExpression(left, right)
    case ElementwiseModulo         => L4_ElementwiseModuloExpression(left, right)

    case AndAnd | AndAndWritten => L4_AndAndExpression(left, right)
    case OrOr | OrOrWritten     => L4_OrOrExpression(left, right)
    case Negation               => L4_NegationExpression(left)
    case EqEq                   => L4_EqEqExpression(left, right)
    case Neq                    => L4_NeqExpression(left, right)
    case Lower                  => L4_LowerExpression(left, right)
    case LowerEqual             => L4_LowerEqualExpression(left, right)
    case Greater                => L4_GreaterExpression(left, right)
    case GreaterEqual           => L4_GreaterEqualExpression(left, right)
  }
}

/// (scalar) arithmetic operations

object L4_AdditionExpression {
  def apply(varargs : L4_Expression*) = new L4_AdditionExpression(varargs.to[ListBuffer])
}

case class L4_AdditionExpression(var summands : ListBuffer[L4_Expression]) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
  override def progress = IR_AdditionExpression(summands.map(_.progress))
}

case class L4_SubtractionExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
  override def progress = IR_SubtractionExpression(left.progress, right.progress)
}

object L4_MultiplicationExpression {
  def apply(varargs : L4_Expression*) = new L4_MultiplicationExpression(varargs.to[ListBuffer])
}

case class L4_MultiplicationExpression(var factors : ListBuffer[L4_Expression]) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
  override def progress = IR_MultiplicationExpression(factors.map(_.progress))
}

case class L4_DivisionExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
  override def progress = IR_DivisionExpression(left.progress, right.progress)
}

case class L4_ModuloExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
  override def progress = IR_ModuloExpression(left.progress, right.progress)
}

case class L4_PowerExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "**" << right
  override def progress = IR_PowerExpression(left.progress, right.progress)
}

/// element-wise arithmetic operations

case class L4_ElementwiseAdditionExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = IR_ElementwiseAdditionExpression(left.progress, right.progress)
}

case class L4_ElementwiseSubtractionExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = IR_ElementwiseSubtractionExpression(left.progress, right.progress)
}

case class L4_ElementwiseMultiplicationExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
  override def progress = IR_ElementwiseMultiplicationExpression(left.progress, right.progress)
}

case class L4_ElementwiseDivisionExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
  override def progress = IR_ElementwiseDivisionExpression(left.progress, right.progress)
}

case class L4_ElementwiseModuloExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
  override def progress = IR_ElementwiseModuloExpression(left.progress, right.progress)
}

case class L4_ElementwisePowerExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')'
  // FIXME: check for integer constant => use pown
  override def progress = IR_ElementwisePowerExpression(left.progress, right.progress)
}

/// logical comparison operations

case class L4_EqEqExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
  override def progress = IR_EqEqExpression(left.progress, right.progress)
}

case class L4_NeqExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
  override def progress = IR_NeqExpression(left.progress, right.progress)
}

case class L4_LowerExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
  override def progress = IR_LowerExpression(left.progress, right.progress)
}

case class L4_GreaterExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
  override def progress = IR_GreaterExpression(left.progress, right.progress)
}

case class L4_LowerEqualExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
  override def progress = IR_LowerEqualExpression(left.progress, right.progress)
}

case class L4_GreaterEqualExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
  override def progress = IR_GreaterEqualExpression(left.progress, right.progress)
}

/// (scalar) logical operations

case class L4_AndAndExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
  override def progress = IR_AndAndExpression(left.progress, right.progress)
}

case class L4_OrOrExpression(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
  override def progress = IR_OrOrExpression(left.progress, right.progress)
}
