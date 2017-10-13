package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.parallelization.api.omp.OMP_WaitForFlag

/// IR_LocalCommunicationFinish

case class IR_LocalCommunicationFinish(
    var field : IR_FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var insideFragLoop : Boolean,
    var cond : Option[IR_Expression]) extends IR_LocalCommunication {

  def waitForLocalComm(neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd IR_Negation(IR_IV_NeighborIsRemote(field.domainIndex, neighbor._1.index)),
          ListBuffer[IR_Statement](
            IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(), IR_AddressOf(IR_IV_LocalCommDone(
              field.field,
              DefaultNeighbors.getOpposingNeigh(neighbor._1).index,
              IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor._1.index))))))),
      true)
  }

  override def expand() : Output[StatementList] = {
    var output = ListBuffer[IR_Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs) return output // nothing to do

    // wait until all neighbors signal that they are finished
    if (!Knowledge.comm_pushLocalData)
      output ++= waitForLocalComm(sendNeighbors)
    else
      output ++= waitForLocalComm(recvNeighbors)

    output
  }
}
