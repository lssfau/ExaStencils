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
import exastencils.field.ir._
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.ir.IR_PotentiallyCritical

case class IR_RemoteSend(
    var field : IR_Field,
    var slot : IR_Expression,
    var neighbor : NeighborInfo,
    var src : IR_Expression,
    var numDataPoints : IR_Expression,
    var datatype : IR_Datatype,
    var concurrencyId : Int) extends IR_Statement with IR_Expandable {

  override def expand() : Output[StatementList] = {
    ListBuffer[IR_Statement](
      IR_PotentiallyCritical(MPI_Send(src, numDataPoints, datatype, IR_IV_NeighborRemoteRank(field.domain.index, neighbor.index),
        MPI_GeneratedTag(IR_IV_CommunicationId(), IR_IV_NeighborFragmentIdx(field.domain.index, neighbor.index), neighbor.index, concurrencyId),
        MPI_Request(field, s"Send_${ concurrencyId }", neighbor.index))),
      IR_Assignment(IR_IV_RemoteReqOutstanding(field, s"Send_${ concurrencyId }", neighbor.index), true))
  }
}

/// IR_CopyToSendBuffer

case class IR_CopyToSendBuffer(
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.layout.numDimsData

  override def expand() : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    def equalLevelCopyLoop() : Unit =
      ret += IR_NoInterpPackingRemote(send = true, field, slot, refinementCase, packInfo, concurrencyId, condition)

    def coarseToFineCopyLoop() : Unit =
      ret += IR_QuadraticInterpPackingC2FRemote(send = true, field, slot, refinementCase, packInfo, concurrencyId, condition)

    def getCopyLoop() = {
      if (Knowledge.refinement_enabled) {
        refinementCase match {
          case RefinementCase.EQUAL =>
            equalLevelCopyLoop()
          case RefinementCase.C2F   =>
            coarseToFineCopyLoop()
          case RefinementCase.F2C   =>
            // TODO: linear interp
            equalLevelCopyLoop()
        }
      } else {
        equalLevelCopyLoop()
      }
    }

    getCopyLoop()

    ret
  }
}
