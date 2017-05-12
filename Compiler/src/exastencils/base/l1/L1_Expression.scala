package exastencils.base.l1

import exastencils.datastructures._

trait L1_Expression extends Node

trait L1_Constant extends L1_Expression

trait L1_Number extends L1_Expression {
  def value : AnyVal
}

case class L1_IntegerConstant(var v : Long) extends L1_Number with L1_Constant {
  override def value = v
}

case class L1_FloatConstant(var v : Double) extends L1_Number with L1_Constant {
  override def value = v
}

case class L1_Access(var v : String) extends L1_Constant

case class L1_UnaryExpression(var operator : String, var exp : L1_Expression) extends L1_Expression

case class L1_BinaryExpression(var operator : String, var left : L1_Expression, var right : L1_Expression) extends L1_Expression

case class L1_OperatorApplication(var identifier : String, var exp : L1_Expression) extends L1_Expression
