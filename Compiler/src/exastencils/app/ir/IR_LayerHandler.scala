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

package exastencils.app.ir

import exastencils.app.LayerHandler
import exastencils.applications.ir.IR_HandleMainApplication
import exastencils.applications.swe.ir.IR_ResolveFragmentNlock
import exastencils.applications.swe.ir.IR_ResolveFragmentOrder
import exastencils.applications.swe.ir.IR_ResolveStationFunctions
import exastencils.base.ExaRootNode
import exastencils.base.ir._
import exastencils.baseExt.ir.ComplexNumbers.IR_ResolveComplexNumbers
import exastencils.baseExt.ir._
import exastencils.boundary.ir.IR_ResolveBoundaryFunctions
import exastencils.communication.IR_SetupDefaultNeighborsWrapper
import exastencils.communication.ir._
import exastencils.config._
import exastencils.domain.ir.IR_DomainFunctions
import exastencils.experimental.ir.IR_ResolveGismoFunctions
import exastencils.field.ir._
import exastencils.globals.ir._
import exastencils.grid.ir._
import exastencils.interfacing.ir._
import exastencils.knowledge.ir._
import exastencils.layoutTransformation.ir.IR_LayoutTansformation
import exastencils.operator.ir.IR_ApplyOffsetToStencilFieldAccess
import exastencils.optimization.ir._
import exastencils.parallelization.api.cuda._
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.api.omp._
import exastencils.performance.ir.IR_AddPerformanceEstimatesWrapper
import exastencils.polyhedron._
import exastencils.prettyprinting.PrintToFile
import exastencils.scheduling._
import exastencils.solver.ir._
import exastencils.stencil.ir._
import exastencils.timing.ir._
import exastencils.util.CImg
import exastencils.util.ir._
import exastencils.visualization.ir.interactive.cimg.IR_ResolveCImgFunctions
import exastencils.visualization.ir.interactive.visit.IR_SetupVisit
import exastencils.visualization.ir.postprocessing.IR_ResolveVisualizationPrinters

/// IR_LayerHandler

trait IR_LayerHandler extends LayerHandler

/// IR_DummyLayerHandler

object IR_DummyLayerHandler extends IR_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  def initialize() : Unit = {}
  def schedule() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// IR_DefaultLayerHandler

object IR_DefaultLayerHandler extends IR_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  override def initialize() : Unit = {
    // TODO: use KnowledgeContainer structure
  }

  override def shutdown() : Unit = {
    // TODO: use KnowledgeContainer structure; remove IR_ClearKnowledge
    IR_ClearKnowledge.apply()
  }

  override def print() : Unit = {
    PrintToFile.apply()
  }

  override def schedule() : Unit = {
    scheduler.register(IR_ProcessInlineKnowledge)

    // add globals - init mpi before cuda since cuda might need mpiRank to choose device
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.mpi_enabled, MPI_AddGlobals))
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.cuda_enabled, CUDA_AddGlobals))

    scheduler.register(IR_SetupDefaultNeighborsWrapper)
    scheduler.register(IR_SetupAllocateDataFunctionWrapper)
    scheduler.register(IR_SetupExternalCopyFunctionsWrapper)

    // setup transformations for communication
    scheduler.register(IR_SetupCommunicationTransformationsWrapper)

    // add remaining nodes
    scheduler.register(IR_AddRemainingNodesWrapper)

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.experimental_mergeCommIntoLoops, IR_MergeCommunicateAndLoop))
    scheduler.register(IR_GeneralSimplifyUntilDoneWrapper) // removes (conditional) calls to communication functions that are not possible
    scheduler.register(IR_SetupCommunicationWrapper(firstCall = true))

    scheduler.register(IR_InferDiagAndInverseCallDataTypesWrapper)
    scheduler.register(IR_HandleMainApplication)
    scheduler.register(IR_ResolveBoundaryFunctions)
    scheduler.register(IR_ResolveFragmentOrder)
    scheduler.register(IR_ResolveFragmentNlock)
    scheduler.register(IR_ResolveReadParameters)
    scheduler.register(IR_ResolveStationFunctions)
    scheduler.register(IR_ResolveCImgFunctions)
    scheduler.register(IR_ResolveCharacteristicsFunctions)
    scheduler.register(IR_ResolveJSONFunctions)
    scheduler.register(IR_ResolveBenchmarkFunctions)
    scheduler.register(IR_ResolveGismoFunctions)
    scheduler.register(IR_ResolveVisualizationPrinters)
    scheduler.register(IR_ResolvePrintWithReducedPrec)
    scheduler.register(IR_AdaptTimerFunctions)

    scheduler.register(IR_ExpandWrapper)

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.experimental_compactBufferAllocation, IR_AdaptAllocateDataFunction))

    scheduler.register(IR_SetupStepsizesWrapper)

    scheduler.register(IR_ResolveIntegrateOnGrid)
    scheduler.register(IR_ResolveEvaluateOnGrid)
    scheduler.register(IR_ResolveVirtualFieldAccesses)

    scheduler.register(IR_ResolveLoopOverPoints)
    scheduler.register(IR_ResolveIntergridIndices)
    scheduler.register(IR_ApplyOffsetToFieldAccess)
    scheduler.register(IR_ApplyOffsetToStencilFieldAccess)
    // simplify indices modified just now, otherwise equality checks will not work later on
    scheduler.register(IR_GeneralSimplify)

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.visit_enable, IR_SetupVisit))

    scheduler.register(IR_StencilConvolutionWrapper)

    scheduler.register(IR_ResolveStencilFunction)

    // resolve new virtual field accesses
    scheduler.register(IR_ResolveIntegrateOnGrid)
    scheduler.register(IR_ResolveEvaluateOnGrid)
    scheduler.register(IR_ResolveVirtualFieldAccesses)
    scheduler.register(IR_ApplyOffsetToFieldAccess)
    scheduler.register(IR_ApplyOffsetToStencilFieldAccess)

    scheduler.register(IR_ResolveComplexAccess)

    scheduler.register(IR_ResolveLoopOverPointsInOneFragment)

    scheduler.register(IR_ResolveLocalSolve)
    scheduler.register(IR_GeneralSimplifyUntilDoneWrapper)

    scheduler.register(IR_ResolveComplexNumbers)

    scheduler.register(IR_PreItMOps)

    scheduler.register(IR_AddPerformanceEstimatesWrapper) // after IR_PreItMOps, before resolve

    scheduler.register(IR_ResolveMatrixOpsWrapper)

    scheduler.register(IR_SetupCommunicationWrapper(firstCall = false)) // handle communication statements generated by loop resolution

    scheduler.register(IR_TypeInferenceWrapper(warnMissingDeclarations = false)) // first sweep to allow for VariableAccess extraction in SplitLoopsForHostAndDevice

    // Prepare all suitable LoopOverDimensions and ContractingLoops. This transformation is applied before resolving
    // ContractingLoops to guarantee that memory transfer statements appear only before and after a resolved
    // ContractingLoop (required for temporal blocking). Leads to better device memory occupancy.
    scheduler.register(ConditionedStrategyContainerWrapper(Knowledge.cuda_enabled, CUDA_PrepareHostCode, CUDA_PrepareMPICode))

    scheduler.register(IR_ResolveContractingLoop)

    scheduler.register(IR_SetupCommunicationWrapper(firstCall = false)) // handle communication statements generated by loop resolution

    scheduler.register(IR_MapStencilAssignments)
    scheduler.register(IR_ResolveFieldAccess)

    scheduler.register(IR_ExpandWrapper)

    scheduler.register(IR_ResolveLoopOverFragments)

    // resolve constant IVs before applying poly opt
    scheduler.register(IR_ResolveConstIVs)
    scheduler.register(IR_SimplifyFloatExpressions)
    scheduler.register(IR_GeneralSimplifyUntilDoneWrapper)

    scheduler.register(IR_DuplicateNodesForCSEWrapper)

    scheduler.register(IR_MergeConditions)
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.poly_optLevel_fine > 0, IR_PolyOpt))
    scheduler.register(IR_ResolveLoopOverDimensions)

    scheduler.register(IR_TypeInferenceWrapper(warnMissingDeclarations = true)) // second sweep for any newly introduced nodes - TODO: check if this is necessary

    // Apply CUDA kernel extraction after polyhedral optimizations to work on optimized ForLoopStatements
    scheduler.register(ConditionedStrategyContainerWrapper(Knowledge.cuda_enabled,
      CUDA_AnnotateLoop,
      CUDA_ExtractHostAndDeviceCode,
      CUDA_AdaptKernelDimensionality,
      CUDA_HandleFragmentLoops,
      CUDA_HandleReductions,
      CUDA_ReplaceStdFunctionCallsWrapper))

    scheduler.register(IR_LayoutTansformation)

    // before converting kernel functions -> requires linearized accesses
    scheduler.register(IR_LinearizeDirectFieldAccess)
    scheduler.register(IR_LinearizeExternalFieldAccess)
    scheduler.register(IR_LinearizeTempBufferAccess)
    scheduler.register(CUDA_LinearizeReductionDeviceDataAccess)
    scheduler.register(IR_LinearizeLoopCarriedCSBufferAccess)

    scheduler.register(IR_SimplifyModulo)

    scheduler.register(CUDA_FunctionConversionWrapper)

    scheduler.register(IR_SimplifyIndexExpressions)

    scheduler.register(IR_ResolveBoundedScalar) // after converting kernel functions -> relies on (unresolved) index offsets to determine loop iteration counts
    scheduler.register(IR_ResolveSlotOperations) // after converting kernel functions -> relies on (unresolved) slot accesses

    scheduler.register(IR_ExpandWrapper)

    scheduler.register(ConditionedSingleStrategyWrapper(!Knowledge.mpi_enabled, MPI_RemoveMPI))

    scheduler.register(IR_GeneralSimplifyUntilDoneWrapper)

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.opt_useAddressPrecalc, IR_AddressPrecalculation))

    scheduler.register(IR_SimplifyFloatExpressions)

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.opt_vectorize, IR_Vectorization))

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.opt_unroll > 1, IR_Unrolling))

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.opt_vectorize, IR_RemoveDupSIMDLoads))

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.data_genVariableFieldSizes, IR_GenerateIndexManipFcts))

    // adapt accesses to device data in case of managed memory
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.cuda_enabled && Knowledge.cuda_useManagedMemory, CUDA_AdaptDeviceAccessesForMM))

    scheduler.register(IR_AddInternalVariables)
    // resolve possibly newly added constant IVs
    scheduler.register(IR_ResolveConstIVs)

    // adapt allocations and de-allocations before expanding
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.cuda_enabled, CUDA_AdaptAllocations))

    scheduler.register(IR_ExpandWrapper)

    // resolve newly added fragment loops
    scheduler.register(IR_ResolveLoopOverFragments)

    scheduler.register(ConditionedStrategyContainerWrapper(Knowledge.mpi_enabled, MPI_AddDatatypeSetup, MPI_AddReductions))

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.omp_enabled, OMP_AddParallelSections))
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.omp_enabled && Platform.omp_version < 4.5, OMP_ResolveMatrixReduction))
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.omp_enabled && Platform.omp_version < 3.1, OMP_ResolveMinMaxReduction))
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.omp_enabled && Knowledge.omp_fixArithmeticReductionOrder, OMP_FixArithmeticReductionOrder))
    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.omp_enabled && Platform.omp_requiresCriticalSections, OMP_AddCriticalSections))

    // one last time
    scheduler.register(IR_ExpandWrapper)

    scheduler.register(exastencils.workaround.Compiler)

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.opt_maxInliningSize > 0, IR_Inlining))

    scheduler.register(ConditionedSingleStrategyWrapper(Knowledge.generateFortranInterface, IR_Fortranify))

    scheduler.register(IR_HACK_TypeAliases)
  }
}

/// IR_AddRemainingNodesWrapper

object IR_AddRemainingNodesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    ExaRootNode.ir_root.nodes ++= List(
      // FunctionCollections
      IR_UtilFunctions(),
      IR_DomainFunctions(),
      IR_CommunicationFunctions(),

      // Util
      IR_Stopwatch(),
      IR_TimerFunctions(),
      CImg() // TODO: only if required
    )

    if (Knowledge.cuda_enabled)
      ExaRootNode.ir_root.nodes += CUDA_KernelFunctions()
  }
}