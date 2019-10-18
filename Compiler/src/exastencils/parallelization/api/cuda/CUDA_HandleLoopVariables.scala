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

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._

/// CUDA_ReplaceLoopVariables

object CUDA_ReplaceLoopVariables extends QuietDefaultStrategy("Replace loop variables with generated kernel variables") {
  var loopVariables = ListBuffer[String]()

  this += new Transformation("Searching", {
    case v @ IR_VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      var newName = CUDA_Kernel.KernelVariablePrefix + CUDA_Kernel.KernelGlobalIndexPrefix + loopVariables.indexOf(name)

      if (v.hasAnnotation(CUDA_Kernel.CUDASharedMemoryAccess)) {
        val field = v.getAnnotation(CUDA_Kernel.CUDASharedMemoryAccess).get.asInstanceOf[String]
        newName = CUDA_Kernel.localThreadId(field, loopVariables.indexOf(name)).name
      }

      IR_VariableAccess(newName, IR_IntegerDatatype)
    case s @ IR_StringLiteral(v @ value) if loopVariables.contains(v)                       =>
      var newName = CUDA_Kernel.KernelVariablePrefix + CUDA_Kernel.KernelGlobalIndexPrefix + loopVariables.indexOf(v)

      if (s.hasAnnotation(CUDA_Kernel.CUDASharedMemoryAccess)) {
        val field = s.getAnnotation(CUDA_Kernel.CUDASharedMemoryAccess).get.asInstanceOf[String]
        newName = CUDA_Kernel.localThreadId(field, loopVariables.indexOf(v)).name
      }

      IR_VariableAccess(newName, IR_IntegerDatatype)
  })
}

/// CUDA_ReplaceLoopVariablesInWrapper

object CUDA_ReplaceLoopVariablesInWrapper extends QuietDefaultStrategy("Replace loop variables in wrapper with provided bounds expressions") {
  var loopVariables = ListBuffer[String]()
  var bounds = ListBuffer[IR_Expression]()

  this += new Transformation("Searching", {
    case IR_StringLiteral(v @ value) if loopVariables.contains(v) =>
      bounds(loopVariables.indexOf(v))

    case IR_VariableAccess(n, IR_IntegerDatatype) if loopVariables.contains(n) =>
      bounds(loopVariables.indexOf(n))
  })
}

/// CUDA_AnnotateLoopVariables

object CUDA_AnnotateLoopVariables extends QuietDefaultStrategy("Annotate loop variables for shared memory access") {
  var loopVariables = ListBuffer[String]()
  var accessName = ""

  this += new Transformation("Searching", {
    case v @ IR_VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      v.annotate(CUDA_Kernel.CUDASharedMemoryAccess, accessName)
      v
    case s @ IR_StringLiteral(v @ value) if loopVariables.contains(v)                       =>
      s.annotate(CUDA_Kernel.CUDASharedMemoryAccess, accessName)
      s
  })
}
