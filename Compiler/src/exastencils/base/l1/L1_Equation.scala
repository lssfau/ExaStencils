package exastencils.base.l1

case class oldL1_Equation(var left : L1_Expression, var right : L1_Expression) extends L1_Definition

case class oldL1_RHS(var identifier : String, var exp : L1_Expression) extends L1_Definition
