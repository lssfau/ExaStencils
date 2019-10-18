//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.boundary.ir

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.boundary.l4._
import exastencils.field.l4._

/// IR_FunctionBC

// wraps a user-provided function implementing boundary handling
case class IR_FunctionBC(var boundaryFunction : IR_FunctionCall) extends IR_BoundaryCondition {}

/// L4_ResolveBoundaryHandlingFunctions

object L4_ResolveBoundaryHandlingFunctions {
  // extends DefaultStrategy("Resolve boundary handling functions") {
  def apply() = {
    for (field <- L4_FieldCollection.objects) {
      field.boundary match {
        case L4_DirichletBC(fctCall : L4_FunctionCall, _) =>
          if (fctCall.function.asInstanceOf[L4_FunctionReference].returnType == L4_UnitDatatype)
            field.boundary = L4_FunctionBC(fctCall)
        case _                                            =>
      }
    }
  }
}
