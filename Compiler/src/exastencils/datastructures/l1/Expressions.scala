package exastencils.datastructures.l1

import exastencils.datastructures._

trait Expression extends Node
trait Constant extends Expression

trait Number extends Expression {
  def value : AnyVal
}

case class IntegerConstant(var v : Long) extends Number with Constant {
  override def value = v
}

case class FloatConstant(var v : Double) extends Number with Constant {
  override def value = v
}

case class Access(var v : String) extends Constant

case class UnaryExpression(var operator : String, var exp : Expression) extends Expression

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression

case class OperatorApplication(var identifier : String, var exp : Expression) extends Expression
