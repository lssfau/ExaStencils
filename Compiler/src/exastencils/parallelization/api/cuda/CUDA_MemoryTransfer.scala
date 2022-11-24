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
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.fieldlike.ir.IR_MultiDimFieldLikeAccess

/// CUDA_TransferUtil

object CUDA_TransferUtil {
  def genTransfer(hostData : IR_InternalVariable, deviceData : IR_InternalVariable, sizeInBytes : IR_Expression, direction : String) : IR_Statement = {
    if (Knowledge.cuda_useManagedMemory) {
      if (Knowledge.cuda_genAsyncPrefetch)
        CUDA_MemPrefetch(hostData, sizeInBytes, direction match {
          case "H2D" => Knowledge.cuda_deviceId
          case "D2H" => "cudaCpuDeviceId"
        })
      else
        IR_NullStatement
    } else {
      direction match {
        case "H2D" => CUDA_Memcpy(deviceData, hostData, sizeInBytes, "cudaMemcpyHostToDevice")
        case "D2H" => CUDA_Memcpy(hostData, deviceData, sizeInBytes, "cudaMemcpyDeviceToHost")
      }
    }
  }
}

/// CUDA_UpdateHostData

object CUDA_UpdateHostData {
  def apply(access : IR_MultiDimFieldLikeAccess) =
    new CUDA_UpdateHostData(IR_IV_AbstractFieldLikeData(access.field, Duplicate(access.slot), Duplicate(access.fragIdx)))
}

case class CUDA_UpdateHostData(var fieldData : IR_IV_AbstractFieldLikeData) extends CUDA_HostStatement with IR_Expandable {
  // TODO: allow targeting of specific index ranges

  override def expand() : Output[IR_Statement] = {
    if (Knowledge.cuda_useZeroCopy || List("both", "device_to_host").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_NullStatement

    val field = fieldData.field

    IR_IfCondition(
      CUDA_DeviceDataUpdated(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          IR_IV_AbstractFieldLikeData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          CUDA_FieldDeviceData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          (0 until field.layout.numDimsData).map(dim => field.layout.idxById("TOT", dim)).reduceLeft(_ * _) * IR_SizeOf(field.resolveBaseDatatype),
          "D2H"),
        IR_Assignment(CUDA_DeviceDataUpdated(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

object CUDA_UpdateDeviceData {
  def apply(access : IR_MultiDimFieldLikeAccess) =
    new CUDA_UpdateDeviceData(IR_IV_AbstractFieldLikeData(access.field, Duplicate(access.slot), Duplicate(access.fragIdx)))
}

case class CUDA_UpdateDeviceData(var fieldData : IR_IV_AbstractFieldLikeData) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    if (Knowledge.cuda_useZeroCopy || List("both", "host_to_device").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_NullStatement

    val field = fieldData.field

    IR_IfCondition(
      CUDA_HostDataUpdated(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          IR_IV_AbstractFieldLikeData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          CUDA_FieldDeviceData(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)),
          (0 until field.layout.numDimsData).map(dim => field.layout.idxById("TOT", dim)).reduceLeft(_ * _) * IR_SizeOf(field.resolveBaseDatatype),
          "H2D"),
        IR_Assignment(CUDA_HostDataUpdated(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateHostBufferData

case class CUDA_UpdateHostBufferData(var buffer : IR_IV_CommBuffer) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    if (Knowledge.cuda_useZeroCopy || List("both", "device_to_host").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_NullStatement

    val field = buffer.field

    IR_IfCondition(
      CUDA_DeviceBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx)),
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          Duplicate(buffer),
          CUDA_BufferDeviceData(field, buffer.direction, Duplicate(buffer.size), Duplicate(buffer.neighIdx)),
          Duplicate(buffer.size) * IR_SizeOf(field.resolveBaseDatatype),
          "D2H"),
        IR_Assignment(CUDA_DeviceBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx)), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

case class CUDA_UpdateDeviceBufferData(var buffer : IR_IV_CommBuffer) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    if (Knowledge.cuda_useZeroCopy || List("both", "host_to_device").contains(Knowledge.cuda_eliminate_memory_transfers))
      return IR_NullStatement

    val field = buffer.field

    IR_IfCondition(
      CUDA_HostBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx)),
      ListBuffer[IR_Statement](
        CUDA_TransferUtil.genTransfer(
          Duplicate(buffer),
          CUDA_BufferDeviceData(field, buffer.direction, Duplicate(buffer.size), Duplicate(buffer.neighIdx)),
          Duplicate(buffer.size) * IR_SizeOf(field.resolveBaseDatatype),
          "H2D"),
        IR_Assignment(CUDA_HostBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx)), IR_BooleanConstant(false))))
  }
}
