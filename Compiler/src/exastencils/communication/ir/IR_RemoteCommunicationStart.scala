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
import exastencils.communication.NeighborInfo
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.domain.ir._
import exastencils.fieldlike.ir._
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.mpi.MPI_DataType

/// IR_RemoteCommunicationStart

case class IR_RemoteCommunicationStart(
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var packInfos : ListBuffer[IR_RemotePackInfo],
    var start : Boolean, var end : Boolean,
    var concurrencyId : Int,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_RemoteCommunication {

  override def genCopy(packInfo : IR_RemotePackInfo, addCondition : Boolean, indexOfRefinedNeighbor : Option[IR_Expression] = None) : IR_Statement = {
    val neighbor = packInfo.neighbor
    val indices = packInfo.getPackInterval()
    val refinementCase = packInfo.refinementCase

    if (requiresPacking(refinementCase, indices, condition)) {
      val body = IR_CopyToSendBuffer(field, Duplicate(slot), refinementCase, packInfo, concurrencyId, indexOfRefinedNeighbor, Duplicate(condition))
      if (addCondition) wrapCond(refinementCase, Duplicate(neighbor), getIndexOfRefinedNeighbor(packInfo), ListBuffer[IR_Statement](body)) else body
    } else {
      IR_NullStatement
    }
  }

  override def genTransfer(packInfo : IR_RemotePackInfo, addCondition : Boolean, indexOfRefinedNeighbor : Option[IR_Expression] = None) : IR_Statement = {
    val neighbor = packInfo.neighbor
    val indices = packInfo.getPackInterval()
    val refinementCase = packInfo.refinementCase

    val body = {
      val totalPackInterval = Duplicate(indices).getTotalSize
      val maxCnt =
        if (refinementCase == RefinementCase.C2F) // actual size after extrapolation
          totalPackInterval * Knowledge.refinement_maxFineNeighborsPerDim
        else if (refinementCase == RefinementCase.F2C) // actual size after interpolation
          totalPackInterval / Knowledge.refinement_maxFineNeighborsPerDim
        else
          totalPackInterval

      val cnt = if (condition.isDefined && Knowledge.comm_compactPackingForConditions)
        IR_IV_CommBufferIterator(field, send = true, neighbor.index, concurrencyId, indexOfRefinedNeighbor)
      else
        maxCnt
      if (field.layout.useFixedLayoutSizes && IR_SimplifyExpression.evalIntegral(maxCnt) <= 0) {
        IR_NullStatement // nothing to do for empty data ranges
      } else if (field.layout.useFixedLayoutSizes && (condition.isEmpty && 1 == IR_SimplifyExpression.evalIntegral(maxCnt))) {
        val arrayAccess = IR_DirectFieldLikeAccess(field, Duplicate(slot), Duplicate(indices.begin)).linearize.expand().inner
        val offsetAccess = IR_PointerOffset(arrayAccess.base, arrayAccess.index)
        IR_RemoteSend(field, Duplicate(slot), Duplicate(neighbor), offsetAccess,
          1, MPI_DataType.determineInnerMPIDatatype(field), concurrencyId, indexOfRefinedNeighbor)
      } else if (MPI_DataType.shouldBeUsed(field, indices, condition)) {
        val arrayAccess = IR_DirectFieldLikeAccess(field, Duplicate(slot), Duplicate(indices.begin)).linearize.expand().inner
        val offsetAccess = IR_PointerOffset(arrayAccess.base, arrayAccess.index)
        IR_RemoteSend(field, Duplicate(slot), Duplicate(neighbor), offsetAccess,
          1, MPI_DataType(field, Duplicate(indices), Duplicate(condition)), concurrencyId, indexOfRefinedNeighbor)
      } else {
        IR_RemoteSend(field, Duplicate(slot), Duplicate(neighbor),
          IR_IV_CommBuffer(field, send = true, Duplicate(maxCnt), neighbor.index, concurrencyId, indexOfRefinedNeighbor),
          cnt, MPI_DataType.determineInnerMPIDatatype(field), concurrencyId, indexOfRefinedNeighbor)
      }
    }
    if (addCondition) wrapCond(refinementCase, Duplicate(neighbor), getIndexOfRefinedNeighbor(packInfo), ListBuffer[IR_Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo, indexOfRefinedNeighbor : Option[IR_Expression] = None) : IR_Statement = {
    IR_WaitForRemoteTransfer(field, send = true, Duplicate(neighbor), concurrencyId, indexOfRefinedNeighbor)
  }

  override def expand() : Output[StatementList] = {
    if (!Knowledge.domain_canHaveRemoteNeighs)
      return ListBuffer[IR_Statement]() // nothing to do

    val domainIdx = field.domain.index

    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      ListBuffer[IR_Statement](
        if (start) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIdx),
            packInfos.map(packInfo => genCopy(packInfo, addCondition = true, getIndexOfRefinedNeighbor(packInfo)))))
        else IR_NullStatement,
        if (start) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIdx),
            packInfos.map(packInfo => genTransfer(packInfo, addCondition = true, getIndexOfRefinedNeighbor(packInfo)))))
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIdx),
            packInfos.map(packInfo => genWait(packInfo.neighbor, getIndexOfRefinedNeighbor(packInfo)))))
        else IR_NullStatement)
    else
      ListBuffer(wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIdx),
          packInfos.map { packInfo =>
            wrapCond(packInfo.refinementCase,
              packInfo.neighbor,
              getIndexOfRefinedNeighbor(packInfo),
              ListBuffer(
                if (start) genCopy(packInfo, addCondition = false, getIndexOfRefinedNeighbor(packInfo)) else IR_NullStatement,
                if (start) genTransfer(packInfo, addCondition = false, getIndexOfRefinedNeighbor(packInfo)) else IR_NullStatement,
                if (end) genWait(packInfo.neighbor, getIndexOfRefinedNeighbor(packInfo)) else IR_NullStatement))
          })))
  }
}
