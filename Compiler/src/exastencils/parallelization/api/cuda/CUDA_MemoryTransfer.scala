package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir._

/// CUDA_UpdateHostData

object CUDA_UpdateHostData {
  def apply(access : IR_MultiDimFieldAccess) = {
    val fieldSelection = access.fieldSelection
    new CUDA_UpdateHostData(IR_IV_FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx))
  }
}

case class CUDA_UpdateHostData(var fieldData : IR_IV_FieldData) extends CUDA_HostStatement with IR_Expandable {
  // TODO: allow targeting of specific index ranges

  override def expand() : Output[IR_IfCondition] = {
    val field = fieldData.field
    IR_IfCondition(
      CUDA_DeviceDataUpdated(field, fieldData.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          IR_IV_FieldData(field, fieldData.level, fieldData.slot),
          CUDA_FieldDeviceData(field, fieldData.level, fieldData.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        IR_Assignment(CUDA_DeviceDataUpdated(field, fieldData.slot), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

object CUDA_UpdateDeviceData {
  def apply(access : IR_MultiDimFieldAccess) = {
    val fieldSelection = access.fieldSelection
    new CUDA_UpdateDeviceData(IR_IV_FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx))
  }
}

case class CUDA_UpdateDeviceData(var fieldData : IR_IV_FieldData) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_IfCondition] = {
    val field = fieldData.field
    IR_IfCondition(
      CUDA_HostDataUpdated(field, fieldData.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          CUDA_FieldDeviceData(field, fieldData.level, fieldData.slot),
          IR_IV_FieldData(field, fieldData.level, fieldData.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        IR_Assignment(CUDA_HostDataUpdated(field, fieldData.slot), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateHostBufferData

case class CUDA_UpdateHostBufferData(var buffer : IR_IV_CommBuffer) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_IfCondition] = {
    val field = buffer.field
    IR_IfCondition(
      CUDA_DeviceBufferDataUpdated(field, buffer.direction, buffer.neighIdx),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          Duplicate(buffer),
          CUDA_BufferDeviceData(field, buffer.direction, buffer.size, buffer.neighIdx),
          buffer.size * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        IR_Assignment(CUDA_DeviceBufferDataUpdated(field, buffer.direction, buffer.neighIdx), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

case class CUDA_UpdateDeviceBufferData(var buffer : IR_IV_CommBuffer) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_IfCondition] = {
    val field = buffer.field
    IR_IfCondition(
      CUDA_HostBufferDataUpdated(field, buffer.direction, buffer.neighIdx),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          CUDA_BufferDeviceData(field, buffer.direction, buffer.size, buffer.neighIdx),
          Duplicate(buffer),
          buffer.size * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        IR_Assignment(CUDA_HostBufferDataUpdated(field, buffer.direction, buffer.neighIdx), IR_BooleanConstant(false))))
  }
}
