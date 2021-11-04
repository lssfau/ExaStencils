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
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir._
import exastencils.config._
import exastencils.domain.ir._
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
import exastencils.performance.ir.IR_AddPerformanceEstimates
import exastencils.polyhedron._
import exastencils.prettyprinting.PrintToFile
import exastencils.solver.ir._
import exastencils.stencil.ir._
import exastencils.timing.ir._
import exastencils.util._
import exastencils.util.ir._
import exastencils.visualization.ir._

/// IR_LayerHandler

trait IR_LayerHandler extends LayerHandler

/// IR_DummyLayerHandler

object IR_DummyLayerHandler extends IR_LayerHandler {
  def initialize() : Unit = {}
  def handle() : Unit = {}
  def print() : Unit = {}
  def shutdown() : Unit = {}
}

/// IR_DefaultLayerHandler

object IR_DefaultLayerHandler extends IR_LayerHandler {
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

  override def handle() : Unit = {
    IR_ProcessInlineKnowledge.apply()

    // add globals - init mpi before cuda since cuda might need mpiRank to choose device
    if (Knowledge.mpi_enabled)
      MPI_AddGlobals.apply()
    if (Knowledge.cuda_enabled)
      CUDA_AddGlobals.apply()

    DefaultNeighbors.setup()
    IR_GlobalCollection.get += IR_AllocateDataFunction(IR_FieldCollection.objects, DefaultNeighbors.neighbors)
    IR_ExternalFieldCollection.generateCopyFunction().foreach(IR_UserFunctions.get += _)

    // setup transformations for communication
    if (Knowledge.comm_enableCommTransformations)
      IR_CommTransformationCollection.setup()

    // add remaining nodes
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

    if (Knowledge.experimental_mergeCommIntoLoops)
      IR_MergeCommunicateAndLoop.apply()
    IR_GeneralSimplify.doUntilDone() // removes (conditional) calls to communication functions that are not possible
    IR_SetupCommunication.firstCall = true
    IR_SetupCommunication.apply()

    IR_InferDiagAndInverseCallDataTypes.doUntilDone()
    IR_HandleMainApplication.apply()
    IR_ResolveBoundaryFunctions.apply()
    IR_ResolveFragmentOrder.apply()
    IR_ResolveFragmentNlock.apply()
    IR_ResolveReadParameters.apply()
    IR_ResolveStationFunctions.apply()
    IR_ResolveCImgFunctions.apply()
    IR_ResolveCharacteristicsFunctions.apply()
    IR_ResolveBenchmarkFunctions.apply()
    IR_ResolveGismoFunctions.apply()
    IR_ResolveVtkPrinters.apply()
    IR_ResolvePrintWithReducedPrec.apply()
    IR_AdaptTimerFunctions.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    // HACK: create discr_h* again if there are no multigrid level and the field size was defined explicitly
    //   currently this works only if all fields are equally sized
    if (Knowledge.domain_rect_generate && Knowledge.maxLevel <= 0) {
      def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb

      val fLayout : Array[IR_FieldLayoutPerDim] = IR_FieldCollection.objects.head.layout.layoutsPerDim
      Knowledge.discr_hx = Array[Double](globalSize.width(0) /
        (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * fLayout(0).numInnerLayers))
      if (Knowledge.dimensionality > 1)
        Knowledge.discr_hy = Array[Double](globalSize.width(1) /
          (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * fLayout(1).numInnerLayers))
      if (Knowledge.dimensionality > 2)
        Knowledge.discr_hz = Array[Double](globalSize.width(2) /
          (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * fLayout(2).numInnerLayers))
    }

    IR_ResolveIntegrateOnGrid.apply()
    IR_ResolveEvaluateOnGrid.apply()
    IR_ResolveVirtualFieldAccesses.apply()

    IR_ResolveLoopOverPoints.apply()
    IR_ResolveIntergridIndices.apply()
    IR_ApplyOffsetToFieldAccess.apply()
    IR_ApplyOffsetToStencilFieldAccess.apply()
    // simplify indices modified just now, otherwise equality checks will not work later on
    IR_GeneralSimplify.apply()

    var convChanged = false
    do {
      IR_FindStencilConvolutions.changed = false
      IR_FindStencilConvolutions.apply()
      convChanged = IR_FindStencilConvolutions.changed

      IR_WrapStencilConvolutions.apply()

      if (Knowledge.useFasterExpand)
        IR_ExpandInOnePass.apply()
      else
        IR_Expand.doUntilDone()
    } while (convChanged)

    IR_ResolveStencilFunction.apply()

    if (Knowledge.experimental_visit_enable)
      IR_SetupVisit.apply()

    // resolve new virtual field accesses
    IR_ResolveIntegrateOnGrid.apply()
    IR_ResolveEvaluateOnGrid.apply()
    IR_ResolveVirtualFieldAccesses.apply()
    IR_ApplyOffsetToFieldAccess.apply()
    IR_ApplyOffsetToStencilFieldAccess.apply()

    IR_ResolveComplexAccess.apply() //TODO: brauche ich für den Complex Access

    IR_ResolveLoopOverPointsInOneFragment.apply()

    IR_ResolveLocalSolve.apply()
    IR_GeneralSimplify.doUntilDone()

    IR_ResolveComplexNumbers.apply()

   IR_PreItMOps.apply()
    //IR_SetupMatrixExpressions.apply()
    var sthChanged = true
    while (sthChanged) {
      IR_GeneralSimplify.doUntilDone()
      IR_ResolveMatFuncs.apply()
      IR_ResolveMatOperators.apply()
      sthChanged = IR_ResolveMatFuncs.results.last._2.matches > 0 || IR_ResolveMatOperators.results.last._2.matches > 0
    }
    IR_GeneralSimplify.doUntilDone()
    IR_PostItMOps.apply()
    IR_LinearizeMatrices.apply()

    IR_SetupCommunication.apply() // handle communication statements generated by loop resolution

    IR_TypeInference.warnMissingDeclarations = false
    IR_TypeInference.apply() // first sweep to allow for VariableAccess extraction in SplitLoopsForHostAndDevice

    if (Knowledge.performance_addEstimation)
      IR_AddPerformanceEstimates.apply()

    // Prepare all suitable LoopOverDimensions and ContractingLoops. This transformation is applied before resolving
    // ContractingLoops to guarantee that memory transfer statements appear only before and after a resolved
    // ContractingLoop (required for temporal blocking). Leads to better device memory occupancy.
    if (Knowledge.cuda_enabled) {
      CUDA_PrepareHostCode.apply()
      CUDA_PrepareMPICode.apply()
    }

    IR_ResolveContractingLoop.apply()

    IR_SetupCommunication.apply() // handle communication statements generated by loop resolution

    IR_MapStencilAssignments.apply()
    IR_ResolveFieldAccess.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    IR_ResolveLoopOverFragments.apply()

    // resolve constant IVs before applying poly opt
    IR_ResolveConstIVs.apply()
    IR_SimplifyFloatExpressions.apply()
    IR_GeneralSimplify.doUntilDone()

    if (Knowledge.opt_conventionalCSE || Knowledge.opt_loopCarriedCSE) {
      DuplicateNodes.instances.clear()
      DuplicateNodes.printStack = false
      DuplicateNodes.apply() // FIXME: only debug
      IR_Inlining.apply(true)
      IR_CommonSubexpressionElimination.apply()
    }

    IR_MergeConditions.apply()
    if (Knowledge.poly_optLevel_fine > 0)
      IR_PolyOpt.apply()
    IR_ResolveLoopOverDimensions.apply()

    IR_TypeInference.apply() // second sweep for any newly introduced nodes - TODO: check if this is necessary

    // Apply CUDA kernel extraction after polyhedral optimizations to work on optimized ForLoopStatements
    if (Knowledge.cuda_enabled) {
      CUDA_AnnotateLoop.apply()
      CUDA_ExtractHostAndDeviceCode.apply()
      CUDA_AdaptKernelDimensionality.apply()
      CUDA_HandleReductions.apply()
      CUDA_ReplaceStdFunctionCalls.apply(Some(CUDA_KernelFunctions.get))
    }

    IR_LayoutTansformation.apply()

    // before converting kernel functions -> requires linearized accesses
    IR_LinearizeDirectFieldAccess.apply()
    IR_LinearizeExternalFieldAccess.apply()
    IR_LinearizeTempBufferAccess.apply()
    CUDA_LinearizeReductionDeviceDataAccess.apply()
    IR_LinearizeLoopCarriedCSBufferAccess.apply()

    IR_SimplifyModulo.apply()

    if (Knowledge.cuda_enabled)
      CUDA_KernelFunctions.get.convertToFunctions()

    IR_SimplifyIndexExpressions.apply()

    IR_ResolveBoundedScalar.apply() // after converting kernel functions -> relies on (unresolved) index offsets to determine loop iteration counts
    IR_ResolveSlotOperations.apply() // after converting kernel functions -> relies on (unresolved) slot accesses

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    if (!Knowledge.mpi_enabled)
      MPI_RemoveMPI.apply()

    IR_GeneralSimplify.doUntilDone()

    if (Knowledge.opt_useAddressPrecalc)
      IR_AddressPrecalculation.apply()

    IR_SimplifyFloatExpressions.apply()

    if (Knowledge.opt_vectorize)
      IR_Vectorization.apply()

    if (Knowledge.opt_unroll > 1)
      IR_Unrolling.apply()

    if (Knowledge.opt_vectorize)
      IR_RemoveDupSIMDLoads.apply()

    if (Knowledge.data_genVariableFieldSizes)
      IR_GenerateIndexManipFcts.apply()

    IR_AddInternalVariables.apply()
    // resolve possibly newly added constant IVs
    IR_ResolveConstIVs.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    // resolve newly added fragment loops
    IR_ResolveLoopOverFragments.apply()

    if (Knowledge.mpi_enabled) {
      MPI_AddDatatypeSetup.apply()
      MPI_AddReductions.apply()
    }

    if (Knowledge.omp_enabled) {
      OMP_AddParallelSections.apply()

      // resolve handling for unsupported matrix reductions
      if (Platform.omp_version < 4.5)
        OMP_ResolveMatrixReduction.apply()

      // resolve min/max reductions for omp versions not supporting them inherently
      if (Platform.omp_version < 3.1)
        OMP_ResolveMinMaxReduction.apply()

      if (Knowledge.omp_fixArithmeticReductionOrder)
        OMP_FixArithmeticReductionOrder.apply()

      if (Platform.omp_requiresCriticalSections)
        OMP_AddCriticalSections.apply()
    }

    // one last time
    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()
    IR_GeneralSimplify.doUntilDone()

    exastencils.workaround.Compiler.apply()

    if (Knowledge.opt_maxInliningSize > 0)
      IR_Inlining.apply()

    if (Knowledge.generateFortranInterface)
      IR_Fortranify.apply()

    IR_HACK_TypeAliases.apply()
  }
}
