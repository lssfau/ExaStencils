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

package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir._

/// CUDA_TransferUtil

object CUDA_TransferUtil {
  def genTransfer(hostData : IR_Expression, deviceData : IR_Expression, sizeInBytes : IR_Expression, direction : String, stream : CUDA_Stream) : IR_Statement = {
    if (Knowledge.cuda_useManagedMemory) {
      if (Knowledge.cuda_genAsyncPrefetch) {
        CUDA_MemPrefetch(hostData, sizeInBytes, direction match {
          case "H2D" => Knowledge.cuda_deviceId
          case "D2H" => "cudaCpuDeviceId"
        },
        if (stream.useNonDefaultStreams) Some(stream) else None)
      } else {
        IR_NullStatement
      }
    } else {
      direction match {
        case "H2D" =>
          if (stream.useNonDefaultStreams)
            CUDA_MemcpyAsync(deviceData, hostData, sizeInBytes, "cudaMemcpyHostToDevice", Some(stream))
          else
            CUDA_Memcpy(deviceData, hostData, sizeInBytes, "cudaMemcpyHostToDevice")
        case "D2H" =>
          if (stream.useNonDefaultStreams)
            CUDA_MemcpyAsync(hostData, deviceData, sizeInBytes, "cudaMemcpyDeviceToHost", Some(stream))
          else
            CUDA_Memcpy(hostData, deviceData, sizeInBytes, "cudaMemcpyDeviceToHost")
      }
    }
  }
}

/// CUDA_UpdateHostData

object CUDA_UpdateHostData {
  def apply(access : IR_MultiDimFieldAccess, stream : CUDA_TransferStream) =
    new CUDA_UpdateHostData(IR_IV_FieldData(access.field, Duplicate(access.slot), Duplicate(access.fragIdx)), stream)
}

case class CUDA_UpdateHostData(var fieldData : IR_IV_FieldData, stream : CUDA_TransferStream) extends CUDA_HostStatement with IR_Expandable {
  // TODO: allow targeting of specific index ranges

  override def expand() : Output[IR_Statement] = {
    val field = fieldData.field
    val isDirty = CUDA_DeviceDataUpdated(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx))

    if (Knowledge.cuda_useZeroCopy || List("both", "device_to_host").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_IfCondition(IR_AndAnd(CUDA_CurrentExecutionMode() Neq CUDA_CPUExecutionMode(), isDirty),
        ListBuffer[IR_Statement](CUDA_DeviceSynchronize()))

    IR_IfCondition(isDirty,
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          IR_IV_FieldData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          CUDA_FieldDeviceData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          (0 until field.layout.numDimsData).map(dim => field.layout.idxById("TOT", dim)).reduceLeft(_ * _) * IR_SizeOf(field.resolveBaseDatatype),
          "D2H",
          stream),
        CUDA_EventRecord(CUDA_PendingStreamTransfers(field, fieldData.fragmentIdx), stream)))
  }
}

/// CUDA_UpdateDeviceData

object CUDA_UpdateDeviceData {
  def apply(access : IR_MultiDimFieldAccess, stream : CUDA_TransferStream) =
    new CUDA_UpdateDeviceData(IR_IV_FieldData(access.field, Duplicate(access.slot), Duplicate(access.fragIdx)), stream)
}

case class CUDA_UpdateDeviceData(var fieldData : IR_IV_FieldData, stream : CUDA_TransferStream) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    val field = fieldData.field
    val isDirty = CUDA_HostDataUpdated(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx))

    if (Knowledge.cuda_useZeroCopy || List("both", "host_to_device").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_IfCondition(IR_AndAnd(CUDA_CurrentExecutionMode() Neq CUDA_GPUExecutionMode(), isDirty),
        ListBuffer[IR_Statement](CUDA_DeviceSynchronize()))

    IR_IfCondition(isDirty,
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          IR_IV_FieldData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          CUDA_FieldDeviceData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          (0 until field.layout.numDimsData).map(dim => field.layout.idxById("TOT", dim)).reduceLeft(_ * _) * IR_SizeOf(field.resolveBaseDatatype),
          "H2D",
          stream),
        CUDA_EventRecord(CUDA_PendingStreamTransfers(field, fieldData.fragmentIdx), stream)))
  }
}

/// CUDA_UpdateHostBufferData

case class CUDA_UpdateHostBufferData(var buffer : IR_IV_CommBuffer, stream : CUDA_TransferStream) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    val field = buffer.field
    val isDirty = CUDA_DeviceBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx))

    if (Knowledge.cuda_useZeroCopy || List("both", "device_to_host").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_IfCondition(IR_AndAnd(CUDA_CurrentExecutionMode() Neq CUDA_CPUExecutionMode(), isDirty),
        ListBuffer[IR_Statement](CUDA_DeviceSynchronize()))

    IR_IfCondition(isDirty,
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          Duplicate(buffer),
          CUDA_BufferDeviceData(field, buffer.direction, Duplicate(buffer.size), Duplicate(buffer.neighIdx)),
          Duplicate(buffer.size) * IR_SizeOf(field.resolveBaseDatatype),
          "D2H",
          stream),
        CUDA_EventRecord(CUDA_PendingStreamTransfers(field, buffer.fragmentIdx), stream)))
  }
}

/// CUDA_UpdateDeviceData

case class CUDA_UpdateDeviceBufferData(var buffer : IR_IV_CommBuffer, stream : CUDA_TransferStream) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    val field = buffer.field
    val isDirty = CUDA_HostBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx))

    if (Knowledge.cuda_useZeroCopy || List("both", "host_to_device").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_IfCondition(IR_AndAnd(CUDA_CurrentExecutionMode() Neq CUDA_GPUExecutionMode(), isDirty),
        ListBuffer[IR_Statement](CUDA_DeviceSynchronize()))

    IR_IfCondition(isDirty,
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          Duplicate(buffer),
          CUDA_BufferDeviceData(field, buffer.direction, Duplicate(buffer.size), Duplicate(buffer.neighIdx)),
          Duplicate(buffer.size) * IR_SizeOf(field.resolveBaseDatatype),
          "H2D",
          stream),
        CUDA_EventRecord(CUDA_PendingStreamTransfers(field, buffer.fragmentIdx), stream)))
  }
}
