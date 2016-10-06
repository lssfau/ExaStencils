package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.knowledge.NeighborInfo
import exastencils.prettyprinting._

/// local communication operations

/// remote communication operations

case class IR_WaitForRemoteTransfer(var field : IR_FieldSelection, var neighbor : NeighborInfo, var direction : String) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand : Output[IR_Statement] = {
    IR_IfCondition(
      iv.RemoteReqOutstanding(field.field, direction, neighbor.index),
      ListBuffer[IR_Statement](
        IR_FunctionCall("waitForMPIReq", IR_AddressofExpression(iv.MpiRequest(field.field, direction, neighbor.index))),
        IR_Assignment(iv.RemoteReqOutstanding(field.field, direction, neighbor.index), false)))
  }
}
