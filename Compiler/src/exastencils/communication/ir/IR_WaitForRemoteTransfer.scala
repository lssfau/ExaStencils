package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.datastructures.Transformation._
import exastencils.field.ir.IR_Field
import exastencils.parallelization.api.mpi._

/// local communication operations

/// remote communication operations

case class IR_WaitForRemoteTransfer(var field : IR_Field, var neighbor : NeighborInfo, var direction : String) extends IR_Statement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    IR_IfCondition(
      IR_IV_RemoteReqOutstanding(field, direction, neighbor.index),
      ListBuffer[IR_Statement](
        IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(MPI_Request(field, direction, neighbor.index))),
        IR_Assignment(IR_IV_RemoteReqOutstanding(field, direction, neighbor.index), false)))
  }
}
