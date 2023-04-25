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
import exastencils.communication._
import exastencils.datastructures.Transformation.Output
import exastencils.domain.ir._
import exastencils.fieldlike.ir._
import exastencils.parallelization.api.omp.OMP_WaitForFlag
import exastencils.timing.ir._

/// IR_LocalSend

case class IR_LocalSend(
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable with IR_ApplyLocalCommunication with IR_HasRefinedPacking {

  def numDims = field.layout.numDimsData

  def indexOfRefinedNeighbor = getIndexOfRefinedNeighbor(packInfo)

  def equalLevelCopyLoop() : IR_Statement =
    IR_NoInterpPackingLocal(send = true, field, slot, refinementCase, packInfo, indexOfRefinedNeighbor, condition)

  def coarseToFineCopyLoop() : IR_Statement =
    IR_QuadraticInterpPackingC2FLocal(send = true, field, slot, refinementCase, packInfo, indexOfRefinedNeighbor, condition)

  def fineToCoarseCopyLoop() : IR_Statement =
    IR_LinearInterpPackingF2CLocal(send = true, field, slot, refinementCase, packInfo, indexOfRefinedNeighbor, condition)

  override def expand() : Output[IR_Statement] = {
    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    val ifCondStmts = ListBuffer[IR_Statement](
      // wait until the fragment to be written to is ready for communication
      IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(),
        IR_AddressOf(IR_IV_LocalCommReady(field,
          DefaultNeighbors.getOpposingNeigh(neighborIdx).index,
          IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor)))),
      getCopyLoop(packInfo.refinementCase),
      // signal other threads that the data reading step is completed
      IR_Assignment(IR_IV_LocalCommDone(field, neighborIdx), IR_BooleanConstant(true)))

    // add automatic timers for packing
    val timingCategory = IR_AutomaticTimingCategory.PACK
    if (IR_AutomaticTimingCategory.categoryEnabled(timingCategory)) {
      val timer = IR_IV_AutomaticTimer(s"autoTime_${ timingCategory.toString }", timingCategory)

      ifCondStmts.prepend(IR_FunctionCall(IR_StartTimer().name, timer))
      ifCondStmts.append(IR_FunctionCall(IR_StopTimer().name, timer))
    }

    IR_IfCondition(
      IR_AndAnd(
        isCurrentFineNeighbor(refinementCase, field.domain.index, neighbor, indexOfRefinedNeighbor),
        isLocalNeighbor(refinementCase, domainIdx, neighborIdx, indexOfRefinedNeighbor)),
      ifCondStmts)
  }
}
