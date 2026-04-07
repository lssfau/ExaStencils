package exastencils.waLBerla.ir.replacements

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.cuda._
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.gpu._

object GPU_WaLBerlaReplaceGPUIVs extends IR_WaLBerlaReplacementStrategy("Replace GPU IVs with waLBerla counterparts") {

  this += Transformation("Replace", {
    // flags
    case _ @ CUDA_HostDataUpdated(field, slot, fragmentIdx) if inWaLBerlaScope(collector) && IR_WaLBerlaFieldCollection.objects.contains(field)   =>
      GPU_WaLBerlaHostDataUpdated(field, slot, field.level, fragmentIdx)
    case _ @ CUDA_DeviceDataUpdated(field, slot, fragmentIdx) if inWaLBerlaScope(collector) && IR_WaLBerlaFieldCollection.objects.contains(field) =>
      GPU_WaLBerlaDeviceDataUpdated(field, slot, field.level, fragmentIdx)

    // TODO: set indexOfRefinedNeighbor correctly
    case _ @ CUDA_HostBufferDataUpdated(field, send, neighIdx, fragmentIdx) if inWaLBerlaScope(collector) && IR_WaLBerlaFieldCollection.objects.contains(field)   =>
      GPU_WaLBerlaHostBufferDataUpdated(field, send, neighIdx, indexOfRefinedNeighbor = None, fragmentIdx)
    case _ @ CUDA_DeviceBufferDataUpdated(field, send, neighIdx, fragmentIdx) if inWaLBerlaScope(collector) && IR_WaLBerlaFieldCollection.objects.contains(field) =>
      GPU_WaLBerlaDeviceBufferDataUpdated(field, send, neighIdx, indexOfRefinedNeighbor = None, fragmentIdx)

    // data
    case _ @ CUDA_BufferDeviceData(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaScope(collector) && IR_WaLBerlaFieldCollection.objects.contains(field) =>
      GPU_WaLBerlaBufferDeviceData(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
  })
}

