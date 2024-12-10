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
import exastencils.timing.ir._

/// IR_RemoteRecv

case class IR_RemoteRecv(
    var field : IR_Field,
    var slot : IR_Expression,
    var neighbor : NeighborInfo,
    var dest : IR_Expression,
    var numDataPoints : IR_Expression,
    var datatype : IR_Datatype,
    var concurrencyId : Int) extends IR_Statement with IR_Expandable {

  override def expand() : Output[StatementList] = {

    ListBuffer[IR_Statement](
      IR_PotentiallyCritical(MPI_Receive(dest, numDataPoints, datatype, IR_IV_NeighborRemoteRank(field.domain.index, neighbor.index),
        MPI_GeneratedTag(IR_IV_NeighborFragmentIdx(field.domain.index, neighbor.index), IR_IV_CommunicationId(),
          if (Knowledge.comm_enableCommTransformations)
            IR_IV_CommNeighNeighIdx(field.domain.index, neighbor.index)
          else
            DefaultNeighbors.getOpposingNeigh(neighbor.index).index, concurrencyId),
        MPI_Request(field, s"Recv_${ concurrencyId }", neighbor.index))),
      IR_Assignment(IR_IV_RemoteReqOutstanding(field, s"Recv_${ concurrencyId }", neighbor.index), true))

  }
}

/// IR_CopyFromRecvBuffer

case class IR_CopyFromRecvBuffer(
    var field : IR_Field,
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
      def it = IR_IV_CommBufferIterator(field, s"Recv_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = IR_TempBufferAccess(IR_IV_CommBuffer(field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = IR_DirectFieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += IR_LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(fieldAccess, tmpBufAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = IR_TempBufferAccess(IR_IV_CommBuffer(field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))

      def fieldAccess = IR_DirectFieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims))

      if (Knowledge.comm_enableCommTransformations) {
        val trafoId = IR_IV_CommTrafoId(field.domain.index, neighbor.index)

        def loop(trafo : IR_CommTransformation) = {
          val ret = new IR_LoopOverDimensions(numDims, indices, ListBuffer[IR_Statement](IR_Assignment(trafo.applyRemoteTrafo(fieldAccess, indices, neighbor), trafo.applyBufferTrafo(tmpBufAccess))))
          ret.polyOptLevel = 1
          ret.parallelization.potentiallyParallel = true
          ret
        }

        ret += IR_Switch(trafoId, IR_CommTransformationCollection.trafos.zipWithIndex.map {
          case (trafo, i) => IR_Case(i, ListBuffer[IR_Statement](loop(trafo)))
        })
      } else {
        val ass : IR_Statement = IR_Assignment(fieldAccess, tmpBufAccess)

        val loop = new IR_LoopOverDimensions(numDims, indices, ListBuffer(ass), condition = condition)
        loop.polyOptLevel = 1
        loop.parallelization.potentiallyParallel = true
        ret += loop
      }

    }

    // add automatic timers for unpacking
    val timingCategory = IR_AutomaticTimingCategory.UNPACK
    if (IR_AutomaticTimingCategory.categoryEnabled(timingCategory)) {
      val timer = IR_IV_AutomaticLeveledTimer(s"autoTime_${ timingCategory.toString }", timingCategory, field.level)

      ret.prepend(IR_FunctionCall(IR_StartTimer().name, timer))
      ret.append(IR_FunctionCall(IR_StopTimer().name, timer))
    }

    ret
  }
}
