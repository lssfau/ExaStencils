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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir._

/// CUDA_UpdateHostData

object CUDA_UpdateHostData {
  def apply(access : IR_MultiDimFieldAccess) =
    new CUDA_UpdateHostData(IR_IV_FieldData(access.field, Duplicate(access.slot), Duplicate(access.fragIdx)))
}

case class CUDA_UpdateHostData(var fieldData : IR_IV_FieldData) extends CUDA_HostStatement with IR_Expandable {
  // TODO: allow targeting of specific index ranges

  override def expand() : Output[IR_IfCondition] = {
    val field = fieldData.field
    IR_IfCondition(
      IR_AndAnd(!List("both", "device_to_host").contains(Knowledge.cuda_eliminate_memory_transfers),
        CUDA_DeviceDataUpdated(field, Duplicate(fieldData.slot))),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          IR_IV_FieldData(field, Duplicate(fieldData.slot)),
          CUDA_FieldDeviceData(field, Duplicate(fieldData.slot)),
          (0 until field.layout.numDimsData).map(dim => field.layout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        IR_Assignment(CUDA_DeviceDataUpdated(field, Duplicate(fieldData.slot)), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

object CUDA_UpdateDeviceData {
  def apply(access : IR_MultiDimFieldAccess) =
    new CUDA_UpdateDeviceData(IR_IV_FieldData(access.field, Duplicate(access.slot), Duplicate(access.fragIdx)))
}

case class CUDA_UpdateDeviceData(var fieldData : IR_IV_FieldData) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_IfCondition] = {
    val field = fieldData.field
    IR_IfCondition(
      IR_AndAnd(!List("both", "host_to_device").contains(Knowledge.cuda_eliminate_memory_transfers),
        CUDA_HostDataUpdated(field, Duplicate(fieldData.slot))),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          CUDA_FieldDeviceData(field, Duplicate(fieldData.slot)),
          IR_IV_FieldData(field, Duplicate(fieldData.slot)),
          (0 until field.layout.numDimsData).map(dim => field.layout.idxById("TOT", dim)).reduceLeft(_ * _)
            * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        IR_Assignment(CUDA_HostDataUpdated(field, Duplicate(fieldData.slot)), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateHostBufferData

case class CUDA_UpdateHostBufferData(var buffer : IR_IV_CommBuffer) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_IfCondition] = {
    val field = buffer.field
    IR_IfCondition(
      IR_AndAnd(!List("both", "device_to_host").contains(Knowledge.cuda_eliminate_memory_transfers),
        CUDA_DeviceBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx))),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          Duplicate(buffer),
          CUDA_BufferDeviceData(field, buffer.direction, Duplicate(buffer.size), Duplicate(buffer.neighIdx)),
          Duplicate(buffer.size) * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        IR_Assignment(CUDA_DeviceBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx)), IR_BooleanConstant(false))))
  }
}

/// CUDA_UpdateDeviceData

case class CUDA_UpdateDeviceBufferData(var buffer : IR_IV_CommBuffer) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_IfCondition] = {
    val field = buffer.field
    IR_IfCondition(
      IR_AndAnd(!List("both", "host_to_device").contains(Knowledge.cuda_eliminate_memory_transfers),
        CUDA_HostBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx))),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          CUDA_BufferDeviceData(field, buffer.direction, Duplicate(buffer.size), Duplicate(buffer.neighIdx)),
          Duplicate(buffer),
          Duplicate(buffer.size) * IR_SizeOf(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        IR_Assignment(CUDA_HostBufferDataUpdated(field, buffer.direction, Duplicate(buffer.neighIdx)), IR_BooleanConstant(false))))
  }
}
