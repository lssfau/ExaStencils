package exastencils.waLBerla.ir.replacements

import exastencils.datastructures.Transformation
import exastencils.parallelization.api.cuda._
import exastencils.waLBerla.ir.gpu._

object IR_WaLBerlaReplaceGPUIVs extends IR_WaLBerlaReplacementStrategy("Replace GPU flags with waLBerla counterparts") {

  this += Transformation("Replace non-recursive", {

    // flags
    case _ @ CUDA_HostDataUpdated(field, slot, fragmentIdx) if inWaLBerlaScope(collector) =>
      GPU_WaLBerlaHostDataUpdated(field, slot, fragmentIdx)
    case _ @ CUDA_DeviceDataUpdated(field, slot, fragmentIdx) if inWaLBerlaScope(collector) =>
      GPU_WaLBerlaDeviceDataUpdated(field, slot, fragmentIdx)

    // TODO: set indexOfRefinedNeighbor correctly
    case _ @ CUDA_HostBufferDataUpdated(field, send, neighIdx, fragmentIdx) if inWaLBerlaScope(collector) =>
      GPU_WaLBerlaHostBufferDataUpdated(field, send, neighIdx, indexOfRefinedNeighbor = None, fragmentIdx)
    // TODO: set indexOfRefinedNeighbor correctly
    case _ @ CUDA_DeviceBufferDataUpdated(field, send, neighIdx, fragmentIdx) if inWaLBerlaScope(collector) =>
      GPU_WaLBerlaDeviceBufferDataUpdated(field, send, neighIdx, indexOfRefinedNeighbor = None, fragmentIdx)

    // data
    case _ @ CUDA_BufferDeviceData(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaScope(collector) =>
      GPU_WaLBerlaBufferDeviceData(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
  })
}

