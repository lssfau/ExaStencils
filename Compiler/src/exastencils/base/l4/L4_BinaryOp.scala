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

  val Maximum = Value("max")
  val Minimum = Value("min")

  //  Conversions for Enumeration:
  // BinaryOperators -> String:  op.toString()
  // String -> BinaryOperators:  BinaryOperators.withName(op)

  def createExpression(op : String, left : L4_Expression, right : L4_Expression) : L4_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : L4_Expression, right : L4_Expression) : L4_Expression = op match {
    case Addition       => L4_Addition(left, right)
    case Subtraction    => L4_Subtraction(left, right)
    case Multiplication => L4_Multiplication(left, right)
    case Division       => L4_Division(left, right)
    case Power          => L4_Power(left, right)
    case Power_Alt      => L4_Power(left, right)
    case Modulo         => L4_Modulo(left, right)

    case ElementwiseAddition       => L4_ElementwiseAddition(left, right)
    case ElementwiseSubtraction    => L4_ElementwiseSubtraction(left, right)
    case ElementwiseMultiplication => L4_ElementwiseMultiplication(left, right)
    case ElementwiseDivision       => L4_ElementwiseDivision(left, right)
    case ElementwisePower          => L4_ElementwisePower(left, right)
    case ElementwiseModulo         => L4_ElementwiseModulo(left, right)

    case AndAnd | AndAndWritten => L4_AndAnd(left, right)
    case OrOr | OrOrWritten     => L4_OrOr(left, right)
    case Negation               => L4_Negation(left)
    case EqEq                   => L4_EqEq(left, right)
    case Neq                    => L4_Neq(left, right)
    case Lower                  => L4_Lower(left, right)
    case LowerEqual             => L4_LowerEqual(left, right)
    case Greater                => L4_Greater(left, right)
    case GreaterEqual           => L4_GreaterEqual(left, right)

    case Maximum => L4_Maximum(left, right)
    case Minimum => L4_Minimum(left, right)
  }

  def progress(op : Value) : IR_BinaryOperators.BinaryOperators = {
    // TODO: better implementation?
    IR_BinaryOperators.withName(op.toString)
  }
}

/// (scalar) arithmetic operations

object L4_Addition {
  def apply(varargs : L4_Expression*) = new L4_Addition(varargs.to[ListBuffer])
}

case class L4_Addition(var summands : ListBuffer[L4_Expression]) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
  override def progress = IR_Addition(summands.map(_.progress))
}

case class L4_Subtraction(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
  override def progress = IR_Subtraction(left.progress, right.progress)
}

object L4_Multiplication {
  def apply(varargs : L4_Expression*) = new L4_Multiplication(varargs.to[ListBuffer])
}

case class L4_Multiplication(var factors : ListBuffer[L4_Expression]) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
  override def progress = IR_Multiplication(factors.map(_.progress))
}

case class L4_Division(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
  override def progress = IR_Division(left.progress, right.progress)
}

case class L4_Modulo(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
  override def progress = IR_Modulo(left.progress, right.progress)
}

case class L4_Power(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "**" << right
  override def progress = IR_Power(left.progress, right.progress)
}

/// element-wise arithmetic operations

case class L4_ElementwiseAddition(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = IR_ElementwiseAddition(left.progress, right.progress)
}

case class L4_ElementwiseSubtraction(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
  override def progress = IR_ElementwiseSubtraction(left.progress, right.progress)
}

case class L4_ElementwiseMultiplication(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
  override def progress = IR_ElementwiseMultiplication(left.progress, right.progress)
}

case class L4_ElementwiseDivision(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
  override def progress = IR_ElementwiseDivision(left.progress, right.progress)
}

case class L4_ElementwiseModulo(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
  override def progress = IR_ElementwiseModulo(left.progress, right.progress)
}

case class L4_ElementwisePower(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')'
  // FIXME: check for integer constant => use pown
  override def progress = IR_ElementwisePower(left.progress, right.progress)
}

/// logical comparison operations

case class L4_EqEq(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
  override def progress = IR_EqEq(left.progress, right.progress)
}

case class L4_Neq(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
  override def progress = IR_Neq(left.progress, right.progress)
}

case class L4_Lower(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
  override def progress = IR_Lower(left.progress, right.progress)
}

case class L4_Greater(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
  override def progress = IR_Greater(left.progress, right.progress)
}

case class L4_LowerEqual(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
  override def progress = IR_LowerEqual(left.progress, right.progress)
}

case class L4_GreaterEqual(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
  override def progress = IR_GreaterEqual(left.progress, right.progress)
}

/// (scalar) logical operations

case class L4_AndAnd(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
  override def progress = IR_AndAnd(left.progress, right.progress)
}

case class L4_OrOr(var left : L4_Expression, var right : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
  override def progress = IR_OrOr(left.progress, right.progress)
}

/// min/max operations

object L4_Minimum {
  def apply(varargs : L4_Expression*) = new L4_Minimum(varargs.to[ListBuffer])
}

case class L4_Minimum(var args : ListBuffer[L4_Expression]) extends L4_Expression {
  override def prettyprint(out : PpStream) = out << "min (" <<< (args, ", ") << ')'
  override def progress = IR_Minimum(args.map(_.progress))
}

object L4_Maximum {
  def apply(varargs : L4_Expression*) = new L4_Maximum(varargs.to[ListBuffer])
}

case class L4_Maximum(var args : ListBuffer[L4_Expression]) extends L4_Expression {
  override def prettyprint(out : PpStream) = out << "max (" <<< (args, ", ") << ')'
  override def progress = IR_Maximum(args.map(_.progress))
}