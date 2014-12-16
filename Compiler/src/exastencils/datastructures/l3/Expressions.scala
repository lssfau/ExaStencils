package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer

import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge

trait Expression extends Node /* with ProgressableToIr {
  def progressToIr : ir.Expression
}*/

trait Number extends Expression {
  def value : AnyVal
}

case class Identifier(var value : String) extends Expression

case class StringConstant(var value : String) extends Expression /* {
  def progressToIr : ir.StringConstant = ir.StringConstant(value)
}*/

case class IntegerConstant(var v : Long) extends Number {
  override def value = v
  //  def progressToIr : ir.IntegerConstant = ir.IntegerConstant(v)
}

case class FloatConstant(var v : Double) extends Number {
  override def value = v
  //  def progressToIr : ir.FloatConstant = ir.FloatConstant(v)
}

case class Variable(var identifier : String, var datatype : Datatype) extends Expression {
  //  def progressToIr : ir.VariableAccess = {
  //    ir.VariableAccess(identifier.progressToIr.asInstanceOf[ir.StringConstant].value, Some(datatype.progressToIr))
  //  }
}

case class FunctionCallExpression(var identifier : String, var arguments : List[Expression]) extends Expression {
  //  def progressToIr : ir.FunctionCallExpression = {
  //    ir.FunctionCallExpression(ir.StringConstant(identifier.progressToIr.asInstanceOf[ir.StringConstant].value),
  //      arguments.map(s => s.progressToIr).to[ListBuffer])
  //  }
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  //  def progressToIr : ir.Expression = {
  //    ir.BinaryOperators.CreateExpression(operator, left.progressToIr, right.progressToIr)
  //  }
}