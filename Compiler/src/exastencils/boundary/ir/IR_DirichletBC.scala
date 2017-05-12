package exastencils.boundary.ir

import exastencils.base.ir.IR_Expression

/// IR_DirichletBC

case class IR_DirichletBC(var boundaryValue : IR_Expression, var order : Int) extends IR_BoundaryCondition {}
