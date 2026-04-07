package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.config.Knowledge
import exastencils.util.ir.IR_RawPrint

object CUDA_DeviceCount {
  def setup() : ListBuffer[IR_Statement] = {
    val deviceCount = CUDA_DeviceCount()

    ListBuffer[IR_Statement](
      IR_FunctionCall(IR_ExternalFunctionReference("cudaGetDeviceCount"), IR_AddressOf(deviceCount)),
      IR_Assert(IR_Lower(Knowledge.cuda_deviceId, deviceCount),
        ListBuffer("\"Invalid device id (\"", Knowledge.cuda_deviceId, "\") must be smaller than the number of devices (\"", deviceCount, "\")\""),
        IR_FunctionCall(IR_ExternalFunctionReference("exit"), 1)),
      s"cudaSetDevice(${ Knowledge.cuda_deviceId })"
    )
  }
}

case class CUDA_DeviceCount() extends IR_UnduplicatedVariable {
  override def resolveName() : String = "deviceCount"
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype

  override def resolveDefValue() : Option[IR_Expression] = Some(IR_IntegerConstant(-1))
}

object CUDA_DeviceProperties {
  def setup() : ListBuffer[IR_Statement] = {
    val deviceProp = CUDA_DeviceProperties()
    val acc = IR_VariableAccess(deviceProp.resolveName(), deviceProp.resolveDatatype())

    ListBuffer[IR_Statement](
      IR_FunctionCall(IR_ExternalFunctionReference("cudaGetDeviceProperties"), IR_AddressOf(deviceProp), s"${ Knowledge.cuda_deviceId }"),
      IR_RawPrint("\"Using CUDA device \"", Knowledge.cuda_deviceId, "\": \"", IR_MemberAccess(acc, "name"), "std::endl"))
  }
}

case class CUDA_DeviceProperties() extends IR_UnduplicatedVariable {
  override def resolveName() : String = "devProp"
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("cudaDeviceProp")
}