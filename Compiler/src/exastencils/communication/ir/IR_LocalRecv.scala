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
import exastencils.fieldlike.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.omp.OMP_WaitForFlag

/// IR_LocalRecv

case class IR_LocalRecv(
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable with IR_ApplyLocalCommunication with IR_HasRefinedPacking {

  def indexOfRefinedNeighbor = getIndexOfRefinedNeighbor(packInfo)

  def numDims = field.layout.numDimsData

  def equalLevelCopyLoop() : IR_Statement =
    IR_NoInterpPackingLocal(send = false, field, slot, refinementCase, packInfo, indexOfRefinedNeighbor, condition)

  def coarseToFineCopyLoop() : IR_Statement =
    IR_QuadraticInterpPackingC2FLocal(send = false, field, slot, refinementCase, packInfo, indexOfRefinedNeighbor, condition)

  def fineToCoarseCopyLoop() : IR_Statement =
    IR_LinearInterpPackingF2CLocal(send = false, field, slot, refinementCase, packInfo, indexOfRefinedNeighbor, condition)

  override def expand() : Output[IR_Statement] = {
    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    def loopWithCommTrafos(trafo : IR_CommTransformation) = {
      val fieldAccess = IR_DirectFieldLikeAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims))
      val neighFieldAccess = IR_DirectFieldLikeAccess(field, Duplicate(slot),
        IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor),
        IR_ExpressionIndex(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), packIntervalSrc.begin, _ + _), packIntervalDest.begin, _ - _))
      val ret = new IR_LoopOverDimensions(numDims, packIntervalDest, ListBuffer[IR_Statement](IR_Assignment(fieldAccess, trafo.applyLocalTrafo(neighFieldAccess, neighbor))))
      ret.polyOptLevel = 1
      ret.parallelization.potentiallyParallel = true

      ret
    }

    var ifCondStmts = ListBuffer[IR_Statement]()
    // wait until the fragment to be read from is ready for communication
    if (Knowledge.comm_enableCommTransformations) {
      if (Knowledge.refinement_enabled)
        Logger.error("Comm transformations are not available with mesh refinement yet.") // TODO

      ifCondStmts += IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(),
        IR_AddressOf(IR_IV_LocalCommReady(field,
          IR_IV_CommNeighNeighIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor),
          IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor)))) // TODO replace getOpposingNeigh
      val trafoId = IR_IV_CommTrafoId(domainIdx, neighborIdx, indexOfRefinedNeighbor)
      ifCondStmts += IR_Switch(trafoId, IR_CommTransformationCollection.trafos.zipWithIndex.map {
        case (trafo, i) => IR_Case(i, ListBuffer[IR_Statement](loopWithCommTrafos(trafo)))
      })
    }
    else {
      ifCondStmts += IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(),
        IR_AddressOf(IR_IV_LocalCommReady(field,
          DefaultNeighbors.getOpposingNeigh(neighborIdx).index,
          IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor))))
      ifCondStmts += getCopyLoop(packInfo.refinementCase)
    }
    // signal other threads that the data reading step is completed
    ifCondStmts += IR_Assignment(IR_IV_LocalCommDone(field, neighborIdx), IR_BooleanConstant(true)) // TODO here too

    IR_IfCondition(isLocalNeighbor(refinementCase, domainIdx, neighborIdx, indexOfRefinedNeighbor),
      ifCondStmts)
  }
}

