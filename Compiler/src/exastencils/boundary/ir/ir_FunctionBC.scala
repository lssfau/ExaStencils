package exastencils.boundary.ir

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.boundary.l4._
import exastencils.field.l4._

/// IR_FunctionBC

// wraps a user-provided function implementing boundary handling
case class IR_FunctionBC(boundaryFunction : IR_FunctionCall) extends IR_BoundaryCondition {}

/// L4_ResolveBoundaryHandlingFunctions

object L4_ResolveBoundaryHandlingFunctions {
  // extends DefaultStrategy("Resolve boundary handling functions") {
  def apply() = {
    for (field <- L4_FieldCollection.objects) {
      field.boundary match {
        case L4_DirichletBC(fctCall : L4_FunctionCall) =>
          if (fctCall.function.asInstanceOf[L4_FunctionAccess].datatype == L4_UnitDatatype)
            field.boundary = L4_FunctionBC(fctCall)
        case _                                         =>
      }
    }
  }
}
