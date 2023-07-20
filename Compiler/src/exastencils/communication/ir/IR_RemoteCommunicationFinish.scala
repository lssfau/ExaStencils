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
import exastencils.field.ir._
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.mpi.MPI_DataType

/// IR_RemoteCommunicationFinish

case class IR_RemoteCommunicationFinish(
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfos : ListBuffer[IR_RemotePackInfo],
    var start : Boolean, var end : Boolean,
    var concurrencyId : Int,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_RemoteCommunication {

  override def genCopy(packInfo : IR_RemotePackInfo, addCondition : Boolean) : IR_Statement = {
    val neighbor = packInfo.neighbor
    val indices = packInfo.getPackInterval()

    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(field, indices, condition) && IR_SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      val body = IR_CopyFromRecvBuffer(field, Duplicate(slot), refinementCase, packInfo, concurrencyId, Duplicate(condition))
      if (addCondition) wrapCond(Duplicate(neighbor), ListBuffer[IR_Statement](body)) else body
    } else {
      IR_NullStatement
    }
  }

  override def genTransfer(packInfo : IR_RemotePackInfo, addCondition : Boolean) : IR_Statement = {
    val neighbor = packInfo.neighbor
    val indices = packInfo.getPackInterval()

    val body = {
      val maxCnt = Duplicate(indices).getTotalSize
      val cnt = maxCnt // always cnt, even when condition is defined -> max count for receive
      if (!Knowledge.data_genVariableFieldSizes && IR_SimplifyExpression.evalIntegral(cnt) <= 0) {
        IR_NullStatement // nothing to do for empty data ranges
      } else if (!Knowledge.data_genVariableFieldSizes && 1 == IR_SimplifyExpression.evalIntegral(cnt)) {
        val arrayAccess = IR_DirectFieldAccess(field, Duplicate(slot), Duplicate(indices.begin)).linearize.expand().inner
        val offsetAccess = IR_PointerOffset(arrayAccess.base, arrayAccess.index)
        IR_RemoteRecv(field, Duplicate(slot), Duplicate(neighbor), offsetAccess, 1, MPI_DataType.determineInnerMPIDatatype(field), concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(field, indices, condition)) {
        val arrayAccess = IR_DirectFieldAccess(field, Duplicate(slot), Duplicate(indices.begin)).linearize.expand().inner
        val offsetAccess = IR_PointerOffset(arrayAccess.base, arrayAccess.index)
        IR_RemoteRecv(field, Duplicate(slot), Duplicate(neighbor), offsetAccess, 1, MPI_DataType(field, Duplicate(indices), Duplicate(condition)), concurrencyId)
      } else {
        IR_RemoteRecv(field, Duplicate(slot), Duplicate(neighbor), IR_IV_CommBuffer(field, s"Recv_${ concurrencyId }", Duplicate(maxCnt), neighbor.index), cnt, MPI_DataType.determineInnerMPIDatatype(field), concurrencyId)
      }
    }
    if (addCondition) wrapCond(Duplicate(neighbor), ListBuffer[IR_Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : IR_Statement = {
    IR_WaitForRemoteTransfer(field, Duplicate(neighbor), s"Recv_${ concurrencyId }")
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
            packInfos.map(pack => genTransfer(pack, true))))
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIdx),
            packInfos.map(pack => genWait(pack.neighbor)))) // TODO: omp parallel or too much overhead? remove inner critical?
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIdx),
            packInfos.map(pack => genCopy(pack, true))))
        else IR_NullStatement)
    else
      ListBuffer(wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIdx), packInfos.map(packInfo =>
          wrapCond(packInfo.neighbor, ListBuffer(
            if (start) genTransfer(packInfo, false) else IR_NullStatement,
            if (end) genWait(packInfo.neighbor) else IR_NullStatement,
            if (end) genCopy(packInfo, false) else IR_NullStatement))))))
  }
}
