package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.NeighborInfo
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.mpi.MPI_DataType

/// IR_RemoteCommunicationStart

case class IR_RemoteCommunicationStart(
    var field : IR_FieldSelection,
    var neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)],
    var start : Boolean, var end : Boolean,
    var concurrencyId : Int,
    var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends IR_RemoteCommunication {

  override def genCopy(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement = {
    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(indices, condition) && IR_SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      val body = IR_CopyToSendBuffer(field, neighbor, indices, concurrencyId, condition)
      if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
    } else {
      IR_NullStatement
    }
  }

  override def genTransfer(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement = {
    val body = {
      val maxCnt = indices.getTotalSize
      val cnt = if (condition.isDefined)
        IR_IV_CommBufferIterator(field.field, s"Send_${ concurrencyId }", neighbor.index)
      else
        maxCnt
      if (!Knowledge.data_genVariableFieldSizes && (condition.isEmpty && 1 == IR_SimplifyExpression.evalIntegral(cnt))) {
        IR_RemoteSend(field, neighbor, IR_AddressOf(IR_DirectFieldAccess(field, indices.begin)), 1, IR_RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices, condition)) {
        IR_RemoteSend(field, neighbor, IR_AddressOf(IR_DirectFieldAccess(field, indices.begin)), 1, MPI_DataType(field, indices, condition), concurrencyId)
      } else {
        IR_RemoteSend(field, neighbor, IR_IV_CommBuffer(field.field, s"Send_${ concurrencyId }", maxCnt, neighbor.index), cnt, IR_RealDatatype, concurrencyId)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : IR_Statement = {
    IR_WaitForRemoteTransfer(field, neighbor, s"Send_${ concurrencyId }")
  }

  override def expand() : Output[StatementList] = {
    if (!Knowledge.domain_canHaveRemoteNeighs)
      return ListBuffer[IR_Statement]() // nothing to do

    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      ListBuffer[IR_Statement](
        if (start) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (start) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genWait(neigh._1))), true)
        else IR_NullStatement)
    else
      ListBuffer(wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex), neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genCopy(neigh._1, neigh._2, false) else IR_NullStatement,
            if (start) genTransfer(neigh._1, neigh._2, false) else IR_NullStatement,
            if (end) genWait(neigh._1) else IR_NullStatement)))), true))
  }
}
