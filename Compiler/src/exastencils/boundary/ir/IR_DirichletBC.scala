package exastencils.boundary.ir

import exastencils.base.ir.IR_Expression

/// IR_DirichletBC

case class IR_DirichletBC(boundaryValue : IR_Expression) extends IR_BoundaryCondition {}
