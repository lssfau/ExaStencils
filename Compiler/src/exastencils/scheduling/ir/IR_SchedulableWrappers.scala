package exastencils.scheduling.ir

import exastencils.base._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.communication.ir._
import exastencils.config._
import exastencils.datastructures.Node
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.globals.ir._
import exastencils.interfacing.ir._
import exastencils.optimization.ir._
import exastencils.parallelization.api.cuda._
import exastencils.performance.ir._
import exastencils.scheduling._
import exastencils.stencil.ir._
import exastencils.timing.ir._
import exastencils.util._
import exastencils.util.ir._

/// IR_SetupDefaultNeighborsWrapper

object IR_SetupDefaultNeighborsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => DefaultNeighbors.setup()
}

/// IR_SetupAllocateDataFunctionWrapper

object IR_SetupAllocateDataFunctionWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => IR_GlobalCollection.get += IR_AllocateDataFunction(IR_FieldCollection.objects, DefaultNeighbors.neighbors)
}

/// IR_SetupExternalCopyFunctionsWrapper

object IR_SetupExternalCopyFunctionsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => IR_ExternalFieldCollection.generateCopyFunction().foreach(IR_UserFunctions.get += _)
}

/// IR_SetupCommunicationTransformationsWrapper

object IR_SetupCommunicationTransformationsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => if (Knowledge.comm_enableCommTransformations) IR_CommTransformationCollection.setup()
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

/// IR_GeneralSimplifyUntilDoneWrapper

object IR_GeneralSimplifyUntilDoneWrapper extends SingleSchedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = {
    IR_GeneralSimplify.doUntilDone(applyAtNode)
  }
}

/// IR_SetupCommunicationWrapper

case class IR_SetupCommunicationWrapper(firstCall : Boolean) extends SingleSchedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = {
    if (firstCall)
      IR_SetupCommunication.firstCall = true
    IR_SetupCommunication.apply(applyAtNode)
  }
}

/// IR_InferDiagAndInverseCallDataTypesWrapper

object IR_InferDiagAndInverseCallDataTypesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => IR_InferDiagAndInverseCallDataTypes.doUntilDone()
}

/// IR_ExpandWrapper

object IR_ExpandWrapper extends SingleSchedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply(applyAtNode)
    else
      IR_Expand.doUntilDone(applyAtNode)
  }
}

/// IR_SetupStepsizesWrapper

object IR_SetupStepsizesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
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
  }
}

/// IR_StencilConvolutionWrapper

object IR_StencilConvolutionWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
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
  }
}

/// IR_AddPerformanceEstimatesWrapper

object IR_AddPerformanceEstimatesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.performance_addEstimation)
      IR_AddPerformanceEstimates.apply()
  }
}

/// IR_ResolveMatrixOpsWrapper

object IR_ResolveMatrixOpsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
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
  }
}

/// IR_TypeInferenceWrapper

case class IR_TypeInferenceWrapper(warnMissingDeclarations : Boolean) extends SingleSchedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = {
    if (!warnMissingDeclarations)
      IR_TypeInference.warnMissingDeclarations = false
    IR_TypeInference.apply(applyAtNode)
  }
}

/// IR_DuplicateNodesForCSEWrapper

object IR_DuplicateNodesForCSEWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.opt_conventionalCSE || Knowledge.opt_loopCarriedCSE) {
      DuplicateNodes.instances.clear()
      DuplicateNodes.printStack = false
      DuplicateNodes.apply() // FIXME: only debug
      IR_Inlining.apply(true)
      IR_CommonSubexpressionElimination.apply()
    }
  }
}

/// CUDA_FunctionConversionWrapper

object CUDA_FunctionConversionWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.cuda_enabled)
      CUDA_KernelFunctions.get.convertToFunctions()
  }
}

/// CUDA_ReplaceStdFunctionCallsWrapper

object CUDA_ReplaceStdFunctionCallsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.cuda_enabled)
      CUDA_ReplaceStdFunctionCalls.apply(Some(CUDA_KernelFunctions.get))
  }
}

/// IR_AddPaddingToFieldLayoutsWrapper

object IR_AddPaddingToFieldLayoutsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.data_alignFieldPointers)
      IR_AddPaddingToFieldLayouts.apply()
  }
}

