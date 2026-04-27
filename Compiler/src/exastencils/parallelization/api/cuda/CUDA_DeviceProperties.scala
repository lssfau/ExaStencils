package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.config.Knowledge
import exastencils.util.ir.IR_RawPrint

abstract class CUDA_IV extends IR_UnduplicatedVariable {
  // default value is not applicable since mpi iv will be initialized in a separate routine
  override def resolveDefValue() = None

  def initialization : ListBuffer[IR_Statement]
}

case object CUDA_DeviceCount extends CUDA_IV {
  override def resolveName() : String = "deviceCount"
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype

  def initialization : ListBuffer[IR_Statement] = {
    val deviceCount = CUDA_DeviceCount

    ListBuffer[IR_Statement](
      IR_FunctionCall(IR_ExternalFunctionReference("cudaGetDeviceCount"), IR_AddressOf(deviceCount)),
      IR_Assert(IR_Lower(Knowledge.cuda_deviceId, CUDA_DeviceCount),
        ListBuffer("\"Invalid device id (\"", Knowledge.cuda_deviceId, "\") must be smaller than the number of devices (\"", deviceCount, "\")\""),
        IR_FunctionCall(IR_ExternalFunctionReference("exit"), 1)),
      s"cudaSetDevice(${ Knowledge.cuda_deviceId })"
    )
  }
}

case object CUDA_DeviceProperties extends CUDA_IV {
  override def resolveName() : String = "devProp"
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("cudaDeviceProp")

  def initialization : ListBuffer[IR_Statement] = {
    val deviceProp = CUDA_DeviceProperties
    val acc = IR_VariableAccess(deviceProp.resolveName(), deviceProp.resolveDatatype())

    ListBuffer[IR_Statement](
      IR_FunctionCall(IR_ExternalFunctionReference("cudaGetDeviceProperties"), IR_AddressOf(deviceProp), s"${ Knowledge.cuda_deviceId }"),
      IR_RawPrint("\"Using CUDA device \"", Knowledge.cuda_deviceId, "\": \"", IR_MemberAccess(acc, "name"), "std::endl"))
  }
}

object CUDA_DeviceSetCacheConfig {
  def initialization : ListBuffer[IR_Statement] = {
    val stmts : ListBuffer[IR_Statement] = ListBuffer()

    // set memory config for device based on knowledge parameters
    if (Knowledge.cuda_useSharedMemory)
      stmts += "cudaDeviceSetCacheConfig(cudaFuncCachePreferShared)"
    if (Knowledge.cuda_favorL1CacheOverSharedMemory)
      stmts += "cudaDeviceSetCacheConfig(cudaFuncCachePreferL1)"

    stmts
  }
}