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

package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.datastructures.Transformation._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.parallelization.api.mpi._

/// local communication operations

/// remote communication operations

case class IR_WaitForRemoteTransfer(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighbor : NeighborInfo,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
) extends IR_RemoteTransfer with IR_HasMessageDirection {

  def expandSpecial() : ListBuffer[IR_Statement] = {
    ListBuffer[IR_Statement](
      IR_IfCondition(
        IR_IV_RemoteReqOutstanding(field, send, neighbor.index, concurrencyId, indexOfRefinedNeighbor),
        ListBuffer[IR_Statement](
          IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(MPI_Request(field, send, neighbor.index, concurrencyId, indexOfRefinedNeighbor))),
          IR_Assignment(IR_IV_RemoteReqOutstanding(field, send, neighbor.index, concurrencyId, indexOfRefinedNeighbor), false))))
  }
}
