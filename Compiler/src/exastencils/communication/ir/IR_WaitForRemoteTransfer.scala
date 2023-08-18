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
import exastencils.field.ir.IR_Field
import exastencils.parallelization.api.mpi._

/// local communication operations

/// remote communication operations

case class IR_WaitForRemoteTransfer(
    var field : IR_Field,
    var send : Boolean,
    var neighbor : NeighborInfo,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[Int],
) extends IR_Statement with IR_Expandable with IR_HasMessageDirection {

  override def expand() : Output[IR_Statement] = {
    IR_IfCondition(
      IR_IV_RemoteReqOutstanding(field, send, neighbor.index, concurrencyId, indexOfRefinedNeighbor),
      ListBuffer[IR_Statement](
        IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(MPI_Request(field, send, neighbor.index, concurrencyId, indexOfRefinedNeighbor))),
        IR_Assignment(IR_IV_RemoteReqOutstanding(field, send, neighbor.index, concurrencyId, indexOfRefinedNeighbor), false)))
  }
}
