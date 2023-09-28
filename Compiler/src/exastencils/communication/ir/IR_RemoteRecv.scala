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
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.domain.ir._
import exastencils.fieldlike.ir._
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.ir.IR_PotentiallyCritical

/// IR_RemoteRecv

case class IR_RemoteRecv(
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var neighbor : NeighborInfo,
    var dest : IR_Expression,
    var numDataPoints : IR_Expression,
    var datatype : IR_Datatype,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression]) extends IR_RemoteTransfer {

  def expandSpecial() = {

    ListBuffer[IR_Statement](
      IR_PotentiallyCritical(
        MPI_Receive(dest, numDataPoints, datatype, IR_IV_NeighborRemoteRank(field.domain.index, neighbor.index, indexOfRefinedNeighbor),
        MPI_GeneratedTag(
          IR_IV_NeighborFragmentIdx(field.domain.index, neighbor.index, indexOfRefinedNeighbor),
          IR_IV_CommunicationId(),
          if (Knowledge.comm_enableCommTransformations)
            IR_IV_CommNeighNeighIdx(field.domain.index, neighbor.index, indexOfRefinedNeighbor)
          else
            DefaultNeighbors.getOpposingNeigh(neighbor.index).index,
          concurrencyId, indexOfRefinedNeighbor),
        MPI_Request(field, send = false, neighbor.index, concurrencyId, indexOfRefinedNeighbor))),
      IR_Assignment(IR_IV_RemoteReqOutstanding(field, send = false, neighbor.index, concurrencyId, indexOfRefinedNeighbor), true))

  }
}

/// IR_CopyFromRecvBuffer

case class IR_CopyFromRecvBuffer(
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable with IR_HasRefinedPacking {

  def numDims = field.layout.numDimsData

  def equalLevelCopyLoop() : IR_Statement =
    IR_NoInterpPackingRemote(send = false, field, slot, refinementCase, packInfo, concurrencyId, indexOfRefinedNeighbor, condition)

  def coarseToFineCopyLoop() : IR_Statement =
    IR_QuadraticInterpPackingC2FRemote(send = false, field, slot, refinementCase, packInfo, concurrencyId, indexOfRefinedNeighbor, condition)

  def fineToCoarseCopyLoop() : IR_Statement =
    IR_LinearInterpPackingF2CRemote(send = false, field, slot, refinementCase, packInfo, concurrencyId, indexOfRefinedNeighbor, condition)

  override def expand() : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    ret += getCopyLoop(packInfo.refinementCase)

    ret
  }
}
