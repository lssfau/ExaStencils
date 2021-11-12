//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.config.Platform
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.ir.IR_ResultingDatatype

/// supported operators

object IR_BinaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerConstant(this)

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
  val RightShift = Value(">>")

  //  Conversions for Enumeration:
  // BinaryOperators -> String:  op.toString()
  // String -> BinaryOperators:  BinaryOperators.withName(op)

  def createExpression(op : String, left : IR_Expression, right : IR_Expression) : IR_Expression = createExpression(withName(op), left, right)
  def createExpression(op : Value, left : IR_Expression, right : IR_Expression) : IR_Expression = op match {
    case Addition       => IR_Addition(left, right)
    case Subtraction    => IR_Subtraction(left, right)
    case Multiplication => IR_Multiplication(left, right)
    case Division       => IR_Division(left, right)
    case Power          => IR_Power(left, right)
    case Power_Alt      => IR_Power(left, right)
    case Modulo         => IR_Modulo(left, right)

    case ElementwiseAddition       => IR_ElementwiseAddition(left, right)
    case ElementwiseSubtraction    => IR_ElementwiseSubtraction(left, right)
    case ElementwiseMultiplication => IR_ElementwiseMultiplication(left, right)
    case ElementwiseDivision       => IR_ElementwiseDivision(left, right)
    case ElementwisePower          => IR_ElementwisePower(left, right)
    case ElementwiseModulo         => IR_ElementwiseModulo(left, right)

    case AndAnd | AndAndWritten => IR_AndAnd(left, right)
    case OrOr | OrOrWritten     => IR_OrOr(left, right)
    case EqEq                   => IR_EqEq(left, right)
    case Neq                    => IR_Neq(left, right)
    case Lower                  => IR_Lower(left, right)
    case LowerEqual             => IR_LowerEqual(left, right)
    case Greater                => IR_Greater(left, right)
    case GreaterEqual           => IR_GreaterEqual(left, right)
    case Maximum                => IR_Maximum(left, right)
    case Minimum                => IR_Minimum(left, right)
    case BitwiseAnd             => IR_BitwiseAnd(left, right)
    case LeftShift              => IR_LeftShift(left, right)
    case RightShift             => IR_RightShift(left, right)
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

object IR_Addition {
  def apply(varargs : IR_Expression*) = new IR_Addition(varargs.to[ListBuffer])
}

case class IR_Addition(var summands : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = summands.view.map(_.datatype).reduce(IR_ResultingDatatype.apply)
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
}

case class IR_Subtraction(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
}

object IR_Multiplication {
  def apply(varargs : IR_Expression*) = new IR_Multiplication(varargs.to[ListBuffer])
}

case class IR_Multiplication(var factors : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = factors.view.map(_.datatype).reduce(IR_ResultingDatatype.apply)
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
}

case class IR_Division(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
}

case class IR_Modulo(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
}

case class IR_Power(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "pow(" << left << ", " << right << ')' // FIXME: check for integer constant => use pown
}

/// element-wise arithmetic operations

case class IR_ElementwiseAddition(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
}

case class IR_ElementwiseSubtraction(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
}

case class IR_ElementwiseMultiplication(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
}

case class IR_ElementwiseDivision(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
}

case class IR_ElementwiseModulo(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
}

case class IR_ElementwisePower(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')' // FIXME: check for integer constant => use pown
}

/// logical comparison operations

case class IR_EqEq(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
}

case class IR_Neq(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
}

case class IR_Lower(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
}

case class IR_Greater(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
}

case class IR_LowerEqual(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
}

case class IR_GreaterEqual(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
}

/// (scalar) logical operations

case class IR_AndAnd(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  // FIXME: convert to listed arguments as for multiplications
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
}

case class IR_OrOr(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
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

object IR_Minimum {
  def apply(varargs : IR_Expression*) = new IR_Minimum(varargs.to[ListBuffer])
}

case class IR_Minimum(var args : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = args.view.map(_.datatype).reduce(IR_ResultingDatatype.apply)

  override def prettyprint(out : PpStream) : Unit = {
    import PrintEnvironment._
    val name = if (out.env == CUDA) "min" else "std::min"
    IR_MinMaxPrinter.prettyprintsb(out, args, name)
  }
}

object IR_Maximum {
  def apply(varargs : IR_Expression*) = new IR_Maximum(varargs.to[ListBuffer])
}

case class IR_Maximum(var args : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = args.view.map(_.datatype).reduce(IR_ResultingDatatype.apply)

  override def prettyprint(out : PpStream) : Unit = {
    import PrintEnvironment._
    val name = if (out.env == CUDA) "max" else "std::max"
    IR_MinMaxPrinter.prettyprintsb(out, args, name)
  }
}

/// other operations

case class IR_BitwiseAnd(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '&' << right << ')'
}

case class IR_LeftShift(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<<" << right << ')'
}

case class IR_RightShift(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">>" << right << ')'
}
