package exastencils.datastructures.l1

import exastencils.datastructures.l1._

case class Equation(var left : Expression, var right : Expression) extends Definition

case class RHS(var identifier : String, var exp : Expression) extends Definition
