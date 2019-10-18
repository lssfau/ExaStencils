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

package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Platform
import exastencils.datastructures._

/// CUDA_AdaptKernelDimensionality

object CUDA_AdaptKernelDimensionality extends DefaultStrategy("Reduce kernel dimensionality where necessary") {
  this += new Transformation("Process kernel nodes", {
    case kernel : CUDA_Kernel =>
      var i : Int = Platform.hw_cuda_maxNumDimsBlock
      while (kernel.parallelDims > Platform.hw_cuda_maxNumDimsBlock) {
        def it = IR_VariableAccess(CUDA_Kernel.KernelVariablePrefix + CUDA_Kernel.KernelGlobalIndexPrefix + i, IR_IntegerDatatype)
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
