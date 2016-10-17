package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir._

/// CUDA_UpdateHostData

case class CUDA_UpdateHostData(var fieldAccess : IR_MultiDimFieldAccess) extends CUDA_HostStatement with IR_Expandable {
  // TODO: allow targeting of specific index ranges

  override def expand() : Output[IR_IfCondition] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    IR_IfCondition(
      CUDA_DeviceDataUpdated(field, fieldSelection.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          IR_IV_FieldData(field, fieldSelection.level, fieldSelection.slot),
          CUDA_FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        IR_Assignment(CUDA_DeviceDataUpdated(field, fieldSelection.slot), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

case class CUDA_UpdateDeviceData(var fieldAccess : IR_MultiDimFieldAccess) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_IfCondition] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    IR_IfCondition(
      CUDA_HostDataUpdated(field, fieldSelection.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          CUDA_FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          IR_IV_FieldData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        IR_Assignment(CUDA_HostDataUpdated(field, fieldSelection.slot), IR_BooleanConstant(false))))
  }
}
