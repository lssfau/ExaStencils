package exastencils.app.ir

import exastencils.baseExt.ir.IR_HACK_TypeAliases
import exastencils.baseExt.ir.IR_ResolveLoopOverFragments
import exastencils.communication.ir.IR_ResolveRemoteTransfer
import exastencils.communication.ir.IR_SetupCommunicationWrapper
import exastencils.config.Knowledge
import exastencils.fieldlike.ir.IR_ResolveFieldLikeAccess
import exastencils.globals.ir.IR_AddInternalVariables
import exastencils.grid.ir.IR_ResolveIntegrateOnGrid
import exastencils.optimization.ir.IR_GeneralSimplifyUntilDoneWrapper
import exastencils.parallelization.api.cuda.CUDA_FunctionConversionWrapper
import exastencils.parallelization.api.cuda.CUDA_PrepareMPICode
import exastencils.scheduling._
import exastencils.waLBerla.ir.blockforest.IR_ResolveWaLBerlaLoopOverBlockNeighborhoodSection
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaResolveLoopOverBlocks
import exastencils.waLBerla.ir.gpu.GPU_WaLBerlaAdaptKernels
import exastencils.waLBerla.ir.gpu.GPU_WaLBerlaHandleGPUMemory
import exastencils.waLBerla.ir.replacements._
import exastencils.waLBerla.ir.interfacing._

object IR_WaLBerlaLayerHandler extends IR_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  override def initialize() : Unit = IR_DefaultLayerHandler.initialize()

  override def schedule() : Unit = {
    IR_DefaultLayerHandler.schedule()
    scheduler.queue ++= IR_DefaultLayerHandler.scheduler.queue

    /* extend schedule of default IR layer handler */

    // append comm replacements
    List(true, false).foreach(b => scheduler.appendToAllFound(IR_SetupCommunicationWrapper(b),
      ConditionedSingleStrategyWrapper(Knowledge.waLBerla_generateCommSchemes, IR_WaLBerlaReplaceCommunication)))

    scheduler.prependToFirstFound(IR_ResolveRemoteTransfer, IR_WaLBerlaReplaceFragmentLoops)

    // replace accesses to geometry information
    scheduler.prependToAllFound(IR_ResolveIntegrateOnGrid,
      ConditionedStrategyContainerWrapper(!Knowledge.waLBerla_useGridPartFromExa,
        IR_WaLBerlaReplaceFragmentLoops,
        IR_WaLBerlaReplaceVirtualFieldAccesses,
        IR_WaLBerlaReplaceCommIVs,
        IR_WaLBerlaReplaceFragmentIVs,
        IR_WaLBerlaReplaceDomainBoundaryConditions))

    // use walberla functions for GPU field memory operations
    scheduler.appendToFirstFound(CUDA_PrepareMPICode,
      ConditionedSingleStrategyWrapper(
        Knowledge.cuda_enabled && Knowledge.waLBerla_useFixedLayoutsFromExa,
        GPU_WaLBerlaHandleGPUMemory))

    // adapt cuda kernels for walberla support
    scheduler.appendToFirstFound(CUDA_FunctionConversionWrapper,
      ConditionedStrategyContainerWrapper(
        Knowledge.cuda_enabled && Knowledge.waLBerla_useFixedLayoutsFromExa,
        GPU_WaLBerlaAdaptKernels,
        GPU_WaLBerlaHandleGPUMemory))

    // resolve block loops before fragment loops are resolved
    scheduler.prependToAllFound(IR_ResolveLoopOverFragments,
      IR_WaLBerlaReplaceFragmentLoops,
      IR_WaLBerlaResolveLoopOverBlocks)

    // resolve block loops before fieldlike accesses are resolved
    scheduler.prependToFirstFound(IR_ResolveFieldLikeAccess,
      IR_WaLBerlaResolveLoopOverBlocks)

    // also add walberla IVs ...
    scheduler.prependToFirstFound(IR_AddInternalVariables,
      IR_WaLBerlaAddInterfaceMembers)

    // ... and adapt memory allocations
    scheduler.appendToFirstFound(IR_AddInternalVariables,
      IR_WaLBerlaReplaceFragmentLoops,
      IR_WaLBerlaReplaceAllocateData)

    // generate interface at last
    scheduler.appendToFirstFound(IR_HACK_TypeAliases,
      IR_ResolveWaLBerlaLoopOverBlockNeighborhoodSection,
      IR_WaLBerlaSetupFunctions,
      IR_WaLBerlaCreateInterface,
      ConditionedSingleStrategyWrapper(!Knowledge.waLBerla_useGridPartFromExa, IR_WaLBerlaReplaceFragmentIVs),
      IR_WaLBerlaReplaceCommIVs,
      IR_WaLBerlaReplaceVariableAccesses,
      IR_WaLBerlaReplaceAllocateData,
      IR_GeneralSimplifyUntilDoneWrapper) // one last time after block loops are expanded and replacements are done
  }

  override def print() : Unit = IR_DefaultLayerHandler.print()

  override def shutdown() : Unit = IR_DefaultLayerHandler.shutdown()
}
