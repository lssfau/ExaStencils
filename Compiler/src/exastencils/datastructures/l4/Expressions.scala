package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge
import exastencils.logger._
import exastencils.prettyprinting._

trait Expression extends Node with ProgressableToIr with PrettyPrintable {
  def progressToIr : ir.Expression
}

trait Number extends Expression {
  def value : AnyVal
}

case class StringConstant(var value : String) extends Expression {
  def this(s : l4.StringLiteral) = this(s.value)
  def prettyprint(out : PpStream) = { out << '\'' << value << '\'' }
  def progressToIr = ir.StringConstant(value)
}

case class StringLiteral(var value : String) extends Expression {
  def this(s : l4.StringConstant) = this(s.value)
  def prettyprint(out : PpStream) = { out << '"' << value << '"' }
  def progressToIr = ir.StringLiteral(value)
}

case class IntegerConstant(var v : Long) extends Number {
  override def value = v

  def prettyprint(out : PpStream) = { out << v }

  def progressToIr : ir.IntegerConstant = ir.IntegerConstant(v)
}

case class FloatConstant(var v : Double) extends Number {
  override def value = v

  def prettyprint(out : PpStream) = {
    out << String.format(java.util.Locale.US, "%s", Double.box(value)) // ensure the compiler can parse the string
  }

  def progressToIr : ir.FloatConstant = ir.FloatConstant(v)
}

case class BooleanConstant(var value : Boolean) extends Expression {
  def prettyprint(out : PpStream) = { out << value }

  def progressToIr : ir.BooleanConstant = ir.BooleanConstant(value)
}

case class VectorExpression(var datatype : Option[Datatype], var expressions : List[Expression], var rowVector : Boolean) extends Expression {
  // rowVector == true: Row; false: Column; None: unspecified
  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.filter(e => e.isInstanceOf[Number]).length == expressions.length

  def prettyprint(out : PpStream) = {
    out << '{' <<< (expressions, ", ") << '}'
    if (!rowVector) {
      out << 'T';
    }
  }
  def progressToIr = new ir.VectorExpression(if (datatype.isDefined) Some(datatype.get.progressToIr); else None, expressions.map(_.progressToIr).to[ListBuffer], rowVector)
}

case class MatrixExpression(var datatype : Option[Datatype], var expressions : List[VectorExpression]) extends Expression {
  if (expressions.filter(x => x.length != expressions(0).length).length > 0) {
    Logger.error("Rows of matrix must be of equal length")
  }

  def prettyprint(out : PpStream) = { out << '{'; expressions.foreach(e => { e.prettyprint(out); out << ",\n" }); out << "} '" }

  def progressToIr = new ir.MatrixExpression(if (datatype.isDefined) Some(datatype.get.progressToIr); else None, expressions.map(_.expressions.map(_.progressToIr).to[ListBuffer]).to[ListBuffer])

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.filter(_.isConstant).length == expressions.length
}

abstract class Identifier extends Expression {
  var name : String
}

case class BasicIdentifier(var name : String) extends Identifier {
  def prettyprint(out : PpStream) = { out << name }

  def progressToIr = ir.StringLiteral(name)
}

case class LeveledIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def prettyprint(out : PpStream) = { out << name << '@' << level }

  def progressToIr = {
    ir.StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class Variable(var identifier : Identifier, var datatype : Datatype) extends Expression {
  def prettyprint(out : PpStream) = { out << identifier }

  def progressToIr = {
    ir.VariableAccess(identifier.progressToIr.asInstanceOf[ir.StringLiteral].value, Some(datatype.progressToIr))
  }
}

case class UnaryExpression(var operator : String, var exp : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << operator << '(' << exp << ')' }

  def progressToIr : ir.Expression = {
    ir.UnaryOperators.CreateExpression(operator, exp.progressToIr)
  }
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << '(' << left << ' ' << operator << ' ' << right << ')' }

  def progressToIr : ir.Expression = {
    this match {
      case BinaryExpression("**", left, IntegerConstant(1)) => left.progressToIr
      case BinaryExpression("**", left, IntegerConstant(2)) => left.progressToIr * Duplicate(left).progressToIr
      case BinaryExpression("**", left, IntegerConstant(3)) => left.progressToIr * Duplicate(left).progressToIr * Duplicate(left).progressToIr
      case _ => ir.BinaryOperators.CreateExpression(operator, left.progressToIr, right.progressToIr)
    }
  }
}

case class BooleanExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << '(' << left << ' ' << operator << ' ' << right << ')' }

  def progressToIr : ir.Expression = {
    ir.BinaryOperators.CreateExpression(operator, left.progressToIr, right.progressToIr)
  }
}

case class UnaryBooleanExpression(var operator : String, var exp : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << '(' << operator << ' ' << exp << ')' }

  def progressToIr : ir.Expression = {
    ir.BinaryOperators.CreateExpression(operator, exp.progressToIr, null) // second argument is ignored
  }
}

case class FunctionCallExpression(var identifier : Access, var arguments : List[Expression]) extends Expression {
  def prettyprint(out : PpStream) = { out << identifier << " ( " <<< (arguments, ", ") << " )" }

  def progressToIr : ir.FunctionCallExpression = {
    ir.FunctionCallExpression(identifier.progressToIr.asInstanceOf[ir.StringLiteral].value,
      arguments.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class StencilConvolution(var stencilAccess : StencilAccess, var fieldAccess : FieldAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilAccess << " * " << fieldAccess }

  def progressToIr : ir.StencilConvolution = {
    ir.StencilConvolution(stencilAccess.getBasicStencilAccess.stencil, fieldAccess.progressToIr)
  }
}

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : FieldAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilFieldAccess << " * " << fieldAccess }

  def progressToIr : ir.StencilFieldConvolution = {
    ir.StencilFieldConvolution(stencilFieldAccess.getBasicStencilFieldAccess, fieldAccess.progressToIr)
  }
}

case class StencilStencilConvolution(var stencilLeft : StencilAccess, var stencilRight : StencilAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progressToIr : ir.StencilStencilConvolution = {
    ir.StencilStencilConvolution(stencilLeft.getBasicStencilAccess.stencil, stencilRight.getBasicStencilAccess.stencil)
  }
}

case class StencilFieldStencilConvolution(var stencilLeft : StencilFieldAccess, var stencilRight : StencilAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progressToIr : ir.StencilFieldStencilConvolution = {
    ir.StencilFieldStencilConvolution(stencilLeft.getBasicStencilFieldAccess, stencilRight.getBasicStencilAccess.stencil)
  }
}
