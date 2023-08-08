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
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.parallelization.api.omp.OMP_WaitForFlag

/// IR_LocalSend

case class IR_LocalSend(
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable with IR_ApplyLocalCommunication {

  def numDims = field.layout.numDimsData

  override def expand() : Output[IR_Statement] = {
    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    def equalLevelCopyLoop() =
      IR_NoInterpPackingLocal(send = true, field, slot, refinementCase, packInfo, condition)

    def coarseToFineCopyLoop() =
      IR_QuadraticInterpPackingC2FLocal(send = true, field, slot, refinementCase, packInfo, condition)

    def getCopyLoop() = {
      if (Knowledge.refinement_enabled) {
        refinementCase match {
          case RefinementCase.EQUAL =>
            equalLevelCopyLoop()
          case RefinementCase.C2F   =>
            coarseToFineCopyLoop()
          case RefinementCase.F2C =>
            // TODO: linear interp
            equalLevelCopyLoop()
        }
      } else {
        equalLevelCopyLoop()
      }
    }

    IR_IfCondition(isLocalNeighbor(refinementCase, domainIdx, neighborIdx),
      ListBuffer[IR_Statement](
        // wait until the fragment to be written to is ready for communication
        IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(), IR_AddressOf(IR_IV_LocalCommReady(
          field, DefaultNeighbors.getOpposingNeigh(neighborIdx).index, IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx)))),
        getCopyLoop(),
        // signal other threads that the data reading step is completed
        IR_Assignment(IR_IV_LocalCommDone(field, neighborIdx), IR_BooleanConstant(true))))
  }
}
