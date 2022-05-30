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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.boundary.l4._
import exastencils.communication.NeighborInfo
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.fieldlike.l4.L4_FieldLikeCollections

/// IR_FunctionBC

// wraps a user-provided function implementing boundary handling
case class IR_FunctionBC(var boundaryFunction : IR_FunctionCall) extends IR_BoundaryCondition {
  override def generateFieldUpdatesNode(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = ListBuffer()
  override def generateFieldUpdatesCell(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = ListBuffer()
}

/// L4_ResolveBoundaryHandlingFunctions

object L4_ResolveBoundaryHandlingFunctions {
  // extends DefaultStrategy("Resolve boundary handling functions") {
  def apply() = {
    for (collection <- L4_FieldLikeCollections.collections) {
      for (field <- collection.objects) {
        field.boundary match {
          case L4_DirichletBC(fctCall : L4_FunctionCall, _) =>
            if (fctCall.function.asInstanceOf[L4_FunctionReference].returnType == L4_UnitDatatype)
              field.boundary = L4_FunctionBC(fctCall)
          case _                                            =>
        }
      }
    }
  }
}
