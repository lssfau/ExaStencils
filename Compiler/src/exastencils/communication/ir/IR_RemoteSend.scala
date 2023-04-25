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
import exastencils.fieldlike.ir.IR_DirectFieldLikeAccess
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.ir.IR_PotentiallyCritical
import exastencils.timing.ir.CommTimerAnnot
import exastencils.timing.ir.IR_IV_Timer
import exastencils.timing.ir.IR_StartTimer
import exastencils.timing.ir.IR_StopTimer

case class IR_RemoteSend(
    var field : IR_FieldLike,
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
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var neighbor : NeighborInfo,
    var indices : IR_ExpressionIndexRange,
    var concurrencyId : Int,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.layout.numDimsData

  override def expand() : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    if (condition.isDefined && Knowledge.comm_compactPackingForConditions) {
      // switch to iterator based copy operation if condition is defined -> number of elements and index mapping is unknown
      def it = IR_IV_CommBufferIterator(field, s"Send_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = IR_TempBufferAccess(IR_IV_CommBuffer(field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = IR_DirectFieldLikeAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += IR_LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(tmpBufAccess, fieldAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = IR_TempBufferAccess(IR_IV_CommBuffer(field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))
      val fieldAccess = IR_DirectFieldLikeAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims))

      val loop = new IR_LoopOverDimensions(numDims, indices, ListBuffer[IR_Statement](IR_Assignment(tmpBufAccess, fieldAccess)))
      loop.polyOptLevel = 1
      loop.parallelization.potentiallyParallel = true
      ret += loop
    }

    if (Knowledge.experimental_measurePackingTimes) {
      val t = IR_IV_Timer("packing")
      val t2 = IR_IV_Timer(s"packing ${ field.codeName }")
      t.annotate(CommTimerAnnot.ANNOT, CommTimerAnnot.COMM)
      t2.annotate(CommTimerAnnot.ANNOT, CommTimerAnnot.COMM)

      ret.prepend(IR_FunctionCall(IR_StartTimer().name, t))
      ret.prepend(IR_FunctionCall(IR_StartTimer().name, t2))

      ret.append(IR_FunctionCall(IR_StopTimer().name, t))
      ret.append(IR_FunctionCall(IR_StopTimer().name, t2))
    }

    ret
  }
}
