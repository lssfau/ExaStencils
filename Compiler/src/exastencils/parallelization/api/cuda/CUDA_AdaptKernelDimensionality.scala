package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Platform
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString

/// CUDA_AdaptKernelDimensionality

object CUDA_AdaptKernelDimensionality extends DefaultStrategy("Reduce kernel dimensionality where necessary") {
  this += new Transformation("Process kernel nodes", {
    case kernel : CUDA_Kernel =>
      var i : Int = Platform.hw_cuda_maxNumDimsBlock
      while (kernel.parallelDims > Platform.hw_cuda_maxNumDimsBlock) {
        def it = IR_VariableAccess(CUDA_Kernel.KernelVariablePrefix + CUDA_Kernel.KernelGlobalIndexPrefix + IR_DimToString(i), IR_IntegerDatatype)
        kernel.body = ListBuffer[IR_Statement](IR_ForLoop(
          IR_VariableDeclaration(it, kernel.lowerBounds(i)),
          IR_Lower(it, kernel.upperBounds(i)),
          IR_Assignment(it, kernel.stepSize(i), "+="),
          kernel.body))
        kernel.parallelDims -= 1
        i += 1
      }
      kernel
  })
}
