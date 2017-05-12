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
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._

/// IR_LocalCommunicationStart

case class IR_LocalCommunicationStart(
    var field : IR_FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var insideFragLoop : Boolean,
    var cond : Option[IR_Expression]) extends IR_LocalCommunication {

  def setLocalCommReady(neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd IR_Negation(IR_IV_NeighborIsRemote(field.domainIndex, neighbor._1.index)),
          IR_Assignment(IR_IV_LocalCommReady(field.field, neighbor._1.index), IR_BooleanConstant(true)))),
      true)
  }

  override def expand() : Output[StatementList] = {
    var output = ListBuffer[IR_Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs) return output // nothing to do

    // set LocalCommReady to signal neighbors readiness for communication
    if (!Knowledge.comm_pushLocalData)
      output ++= setLocalCommReady(sendNeighbors)
    else
      output ++= setLocalCommReady(recvNeighbors)

    if (Knowledge.comm_pushLocalData) {
      // distribute this fragment's data - if enabled
      output += wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
          sendNeighbors.map(neigh =>
            IR_LocalSend(Duplicate(field), Duplicate(neigh._1), Duplicate(neigh._2), Duplicate(neigh._3), insideFragLoop, Duplicate(cond)) : IR_Statement)),
        true)
    } else {
      // pull data for this fragment - otherwise
      output += wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
          recvNeighbors.map(neigh =>
            IR_LocalRecv(Duplicate(field), Duplicate(neigh._1), Duplicate(neigh._2), Duplicate(neigh._3), insideFragLoop, Duplicate(cond)) : IR_Statement)),
        true)
    }

    output
  }
}
