package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer

import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge

trait Expression extends Node {

  def lEval(env : Environment) : LValue = {
    throw new Exception("Cannot evaluate to an static l-value.")
  }
  def rEval(env : Environment) : RValue = {
    throw new Exception("Cannot evaluate to an static r-value.")
  }
  def dynamicLEval(env : Environment) : DynamicLValue = {
    throw new Exception("Cannot evaluate to a dynamic l-value.")
  }
  def dynamicREval(env : Environment) : DynamicRValue = {
    throw new Exception("Cannot evaluate to a dynamic r-value.")
  }

}

trait Number extends Expression {
  def value : AnyVal
}

case class IdentifierExpression(val id : String) extends Expression {

  override def lEval(env : Environment) : LValue = {
    /// @todo
    ???
  }

}

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

case class Variable(val id : String, val datatype : ScType) extends Expression {

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