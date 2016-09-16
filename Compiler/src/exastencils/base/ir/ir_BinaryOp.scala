package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.knowledge.Platform
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.ir.IR_ResultingDatatype

/// supported operators

object IR_BinaryOperators extends Enumeration {
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
  val EqEq = Value("==")
  val Neq = Value("!=")
  val Lower = Value("<")
  val LowerEqual = Value("<=")
  val Greater = Value(">")
  val GreaterEqual = Value(">=")
  val Maximum = Value("max")
  val Minimum = Value("min")
  val BitwiseAnd = Value("&")
  val LeftShift = Value("<<")

  //  Conversions for Enumeration:
  // BinaryOperators -> String:  op.toString()
  // String -> BinaryOperators:  BinaryOperators.withName(op)

  def createExpression(op : String, left : IR_Expression, right : IR_Expression) : IR_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : IR_Expression, right : IR_Expression) : IR_Expression = op match {
    case Addition       => IR_AdditionExpression(left, right)
    case Subtraction    => IR_SubtractionExpression(left, right)
    case Multiplication => IR_MultiplicationExpression(left, right)
    case Division       => IR_DivisionExpression(left, right)
    case Power          => IR_PowerExpression(left, right)
    case Power_Alt      => IR_PowerExpression(left, right)
    case Modulo         => IR_ModuloExpression(left, right)

    case ElementwiseAddition       => IR_ElementwiseAdditionExpression(left, right)
    case ElementwiseSubtraction    => IR_ElementwiseSubtractionExpression(left, right)
    case ElementwiseMultiplication => IR_ElementwiseMultiplicationExpression(left, right)
    case ElementwiseDivision       => IR_ElementwiseDivisionExpression(left, right)
    case ElementwisePower          => IR_ElementwisePowerExpression(left, right)
    case ElementwiseModulo         => IR_ElementwiseModuloExpression(left, right)

    case AndAnd | AndAndWritten => IR_AndAndExpression(left, right)
    case OrOr | OrOrWritten     => IR_OrOrExpression(left, right)
    case EqEq                   => IR_EqEqExpression(left, right)
    case Neq                    => IR_NeqExpression(left, right)
    case Lower                  => IR_LowerExpression(left, right)
    case LowerEqual             => IR_LowerEqualExpression(left, right)
    case Greater                => IR_GreaterExpression(left, right)
    case GreaterEqual           => IR_GreaterEqualExpression(left, right)
    case Maximum                => IR_MaximumExpression(left, right)
    case Minimum                => IR_MinimumExpression(left, right)
    case BitwiseAnd             => IR_BitwiseAndExpression(left, right)
    case LeftShift              => IR_LeftShiftExpression(left, right)
  }

  def opAsIdent(op : String) = {
    op match {
      case "+"  => "Addition"
      case "-"  => "Subtraction"
      case "*"  => "Multiplication"
      case "/"  => "Division"
      case "**" => "Power"
      case "^"  => "Power_Alt"
      case "%"  => "Modulo"

      case ".+"  => "ElementwiseAddition"
      case ".-"  => "ElementwiseSubtraction"
      case ".*"  => "ElementwiseMultiplication"
      case "./"  => "ElementwiseDivision"
      case ".**" => "ElementwisePower"
      case ".%"  => "ElementwiseModulo"

      case "&&"  => "AndAnd"
      case "and" => "AndAndWritten"
      case "||"  => "OrOr"
      case "or"  => "OrOrWritten"
      case "!"   => "Negation"
      case "=="  => "EqEq"
      case "!="  => "Neq"
      case "<"   => "Lower"
      case "<="  => "LowerEqual"
      case ">"   => "Greater"
      case ">="  => "GreaterEqual"
      case "max" => "Maximum"
      case "min" => "Minimum"
      case "&"   => "BitwiseAnd"

      case _ => Logger.warn(s"Unknown op $op"); op
    }
  }
}

/// (scalar) arithmetic operations

object IR_AdditionExpression {
  def apply(varargs : IR_Expression*) = new IR_AdditionExpression(varargs.to[ListBuffer])
}

case class IR_AdditionExpression(var summands : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = {
    var ret = summands(0).datatype
    summands.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
    ret
  }
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
}

case class IR_SubtractionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
}

object IR_MultiplicationExpression {
  def apply(varargs : IR_Expression*) = new IR_MultiplicationExpression(varargs.to[ListBuffer])
}

case class IR_MultiplicationExpression(var factors : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = {
    var ret = factors(0).datatype
    factors.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
    ret
  }
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
}

case class IR_DivisionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
}

case class IR_ModuloExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
}

case class IR_PowerExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "pow(" << left << ", " << right << ')' // FIXME: check for integer constant => use pown
}

/// element-wise arithmetic operations

case class IR_ElementwiseAdditionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
}

case class IR_ElementwiseSubtractionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
}

case class IR_ElementwiseMultiplicationExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
}

case class IR_ElementwiseDivisionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
}

case class IR_ElementwiseModuloExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
}

case class IR_ElementwisePowerExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')' // FIXME: check for integer constant => use pown
}

/// logical comparison operations

case class IR_EqEqExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
}

case class IR_NeqExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
}

case class IR_LowerExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
}

case class IR_GreaterExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
}

case class IR_LowerEqualExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
}

case class IR_GreaterEqualExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
}

/// (scalar) logical operations

case class IR_AndAndExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  // FIXME: convert to listed arguments as for multiplications
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
}

case class IR_OrOrExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  // FIXME: convert to listed arguments as for additions
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
}

/// min/max operations

private object IR_MinMaxPrinter {
  def prettyprintsb(out : PpStream, args : ListBuffer[IR_Expression], method : String) : Unit = {
    if (args.length == 1)
      out << args(0)

    else if (Platform.supports_initializerList && PrintEnvironment.CPP == out.env)
      out << method << "({" <<< (args, ",") << "})"

    else {
      for (i <- 0 until args.length - 1)
        out << method << '('
      val it : Iterator[IR_Expression] = args.iterator
      out << it.next()
      while (it.hasNext)
        out << ',' << it.next() << ')'
    }
  }
}

object IR_MinimumExpression {
  def apply(varargs : IR_Expression*) = new IR_MinimumExpression(varargs.to[ListBuffer])
}

case class IR_MinimumExpression(var args : ListBuffer[IR_Expression]) extends IR_Expression {
  def this(varargs : IR_Expression*) = this(varargs.to[ListBuffer])

  override def datatype = {
    var ret = args(0).datatype
    args.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
    ret
  }

  override def prettyprint(out : PpStream) : Unit = {
    import PrintEnvironment._
    val name = if (out.env == CUDA) "min" else "std::min"
    IR_MinMaxPrinter.prettyprintsb(out, args, name)
  }
}

object IR_MaximumExpression {
  def apply(varargs : IR_Expression*) = new IR_MaximumExpression(varargs.to[ListBuffer])
}

case class IR_MaximumExpression(var args : ListBuffer[IR_Expression]) extends IR_Expression {
  def this(varargs : IR_Expression*) = this(varargs.to[ListBuffer])

  override def datatype = {
    var ret = args(0).datatype
    args.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
    ret
  }

  override def prettyprint(out : PpStream) : Unit = {
    import PrintEnvironment._
    val name = if (out.env == CUDA) "max" else "std::max"
    IR_MinMaxPrinter.prettyprintsb(out, args, name)
  }
}

/// other operations

case class IR_BitwiseAndExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '&' << right << ')'
}

case class IR_LeftShiftExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<<" << right << ')'
}
