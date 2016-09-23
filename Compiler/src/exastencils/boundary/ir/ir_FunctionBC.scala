package exastencils.boundary.ir

import exastencils.base.ir._

/// IR_FunctionBC

// wraps a user-provided function implementing boundary handling
case class IR_FunctionBC(boundaryFunction : IR_FunctionCall) extends IR_BoundaryCondition {}
