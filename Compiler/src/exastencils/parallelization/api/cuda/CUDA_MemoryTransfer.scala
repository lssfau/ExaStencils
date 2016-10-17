package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.iv
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.prettyprinting.PpStream

/// CUDA_UpdateHostData

case class CUDA_UpdateHostData(var fieldAccess : IR_MultiDimFieldAccess) extends CUDA_HostStatement with IR_Expandable {
  // TODO: allow targeting of specific index ranges

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_IfCondition] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    IR_IfCondition(
      iv.DeviceDataUpdated(field, fieldSelection.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
          iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        IR_Assignment(iv.DeviceDataUpdated(field, fieldSelection.slot), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

case class CUDA_UpdateDeviceData(var fieldAccess : IR_MultiDimFieldAccess) extends CUDA_HostStatement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_IfCondition] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    IR_IfCondition(
      iv.HostDataUpdated(field, fieldSelection.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        IR_Assignment(iv.HostDataUpdated(field, fieldSelection.slot), IR_BooleanConstant(false))))
  }
}
