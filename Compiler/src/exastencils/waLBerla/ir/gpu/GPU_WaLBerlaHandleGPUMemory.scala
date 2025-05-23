package exastencils.waLBerla.ir.gpu

import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.logger.Logger
import exastencils.parallelization.api.cuda._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.field.IR_IV_WaLBerlaGetFieldData
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection

object GPU_WaLBerlaHandleGPUMemory extends DefaultStrategy("GPU memory handling for waLBerla fields") {

  def getWaLBerlaFieldOption(fieldData : IR_IV_AbstractFieldLikeData) = IR_WaLBerlaFieldCollection.getByIdentifier(fieldData.field.name, fieldData.field.level)

  this += Transformation("Replace transfers", {
    case _ @ CUDA_MemPrefetch(pointer : IR_IV_AbstractFieldLikeData, _, direction, None) if getWaLBerlaFieldOption(pointer).isDefined =>
      val wbField = getWaLBerlaFieldOption(pointer).get
      val cpuID = IR_WaLBerlaBlockDataID(wbField, pointer.slot, onGPU = false)
      val gpuID = IR_WaLBerlaBlockDataID(wbField, pointer.slot, onGPU = true)
      direction match {
        case id if id == Knowledge.cuda_deviceId => GPU_WaLBerlaFieldCpy(gpuID, cpuID) // H2D
        case "cudaCpuDeviceId"                   => GPU_WaLBerlaFieldCpy(cpuID, gpuID) // D2H
        case _                                   => Logger.error("Unknown direction for CUDA_MemoryPrefetch!")
      }

    case _ @ CUDA_Memcpy(dst : IR_IV_AbstractFieldLikeData, src : IR_IV_AbstractFieldLikeData, _, direction) if getWaLBerlaFieldOption(dst).isDefined && getWaLBerlaFieldOption(src).isDefined =>
      val wbFieldSrc = getWaLBerlaFieldOption(src).get
      val wbFieldDst = getWaLBerlaFieldOption(dst).get

      val (srcOnGPU, dstOnGPU) = direction match {
        case "cudaMemcpyDeviceToHost" => (true, false)
        case "cudaMemcpyHostToDevice" => (false, true)
        case _ => Logger.error("Unknown direction for CUDA_Memcpy!")
      }
      val srcID = IR_WaLBerlaBlockDataID(wbFieldSrc, src.slot, srcOnGPU)
      val dstID = IR_WaLBerlaBlockDataID(wbFieldDst, dst.slot, dstOnGPU)

      GPU_WaLBerlaFieldCpy(dstID, srcID)

    // waLBerla does not provide a function for this
    case _ @ CUDA_MemcpyAsync(dst : IR_IV_AbstractFieldLikeData, src : IR_IV_AbstractFieldLikeData, sizeInBytes, direction, stream) if getWaLBerlaFieldOption(dst).isDefined && getWaLBerlaFieldOption(src).isDefined =>
      val wbFieldSrc = getWaLBerlaFieldOption(src).get
      val wbFieldDst = getWaLBerlaFieldOption(dst).get

      val (srcOnGPU, dstOnGPU) = direction match {
        case "cudaMemcpyDeviceToHost" => (true, false)
        case "cudaMemcpyHostToDevice" => (false, true)
        case _                        => Logger.error("Unknown direction for CUDA_Memcpy!")
      }
      val srcFieldPtr = IR_IV_WaLBerlaGetFieldData(wbFieldSrc, src.slot, srcOnGPU, src.fragmentIdx)
      val dstFieldPtr = IR_IV_WaLBerlaGetFieldData(wbFieldDst, dst.slot, dstOnGPU, dst.fragmentIdx)

      CUDA_MemcpyAsync(dstFieldPtr, srcFieldPtr, sizeInBytes, direction, stream)
  })
}
