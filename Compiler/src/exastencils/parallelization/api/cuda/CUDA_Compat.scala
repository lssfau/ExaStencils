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

import exastencils.base.ir.IR_Node
import exastencils.config._
import exastencils.prettyprinting._

/// CUDA_Compat
///
/// Thin compatibility layer between the CUDA runtime API and HIP
/// The code generator always emits CUDA API calls
/// -> For HIP targets these are re-wired to their HIP counterparts by the generated header below

object CUDA_Compat {
  def defHeader = "Util/GPUCompat.h"

  /// header providing the block-wide reduction primitives used by CUDA_Kernel
  def defCubHeader = "Util/GPUCub.h"

  /// CUDA runtime API symbols emitted by the code generator
  private def cudaSymbols = List(
    // error handling
    "cudaError_t", "cudaSuccess", "cudaGetErrorString", "cudaGetLastError",
    // device handling
    "cudaDeviceProp", "cudaGetDevice", "cudaSetDevice", "cudaGetDeviceCount", "cudaGetDeviceProperties",
    "cudaDeviceSynchronize", "cudaDeviceSetCacheConfig", "cudaFuncCachePreferShared", "cudaFuncCachePreferL1",
    "cudaCpuDeviceId",
    // memory management
    "cudaMalloc", "cudaMallocManaged", "cudaMallocHost", "cudaHostAlloc", "cudaHostAllocMapped",
    "cudaHostGetDevicePointer", "cudaFree", "cudaFreeHost", "cudaMemset",
    // memory transfer
    "cudaMemcpy", "cudaMemcpyAsync", "cudaMemcpyHostToDevice", "cudaMemcpyDeviceToHost", "cudaMemcpyDeviceToDevice",
    "cudaMemPrefetchAsync",
    // streams
    "cudaStream_t", "cudaStreamCreate", "cudaStreamDestroy", "cudaStreamSynchronize", "cudaStreamWaitEvent",
    // events
    "cudaEvent_t", "cudaEventCreateWithFlags", "cudaEventDestroy", "cudaEventRecord", "cudaEventSynchronize",
    "cudaEventDisableTiming")

  /// symbols whose HIP counterpart is not a plain "cuda" -> "hip" rename
  private def hipRenames = Map(
    "cudaDeviceProp" -> "hipDeviceProp_t",
    "cudaMallocHost" -> "hipHostMalloc",
    "cudaHostAlloc" -> "hipHostMalloc",
    "cudaHostAllocMapped" -> "hipHostMallocMapped",
    "cudaFreeHost" -> "hipHostFree")

  /// HIP counterpart of a CUDA runtime API symbol
  private def hipSymbol(cudaSymbol : String) = hipRenames.getOrElse(cudaSymbol, "hip" + cudaSymbol.stripPrefix("cuda"))
}

case class CUDA_Compat() extends IR_Node with FilePrettyPrintable {
  override def printToFile() : Unit = {
    if (!Knowledge.gpu_enabled)
      return

    val writer = PrettyprintingManager.getPrinter(CUDA_Compat.defHeader)

    if (Knowledge.hip_enabled) {
      writer.addExternalDependency("hip/hip_runtime.h")

      writer <<< "// re-wire the CUDA runtime API to HIP"
      val padding = CUDA_Compat.cudaSymbols.map(_.length).max
      CUDA_Compat.cudaSymbols.foreach(sym =>
        writer <<< s"#define ${ sym.padTo(padding, ' ') } ${ CUDA_Compat.hipSymbol(sym) }")
    } else {
      writer.addExternalDependency("cuda.h")
      writer.addExternalDependency("cuda_runtime.h")
    }

    // provide CUB (or its HIP equivalent) if it is used for block-wide reductions
    if (!Knowledge.gpu_useDefaultReductions) {
      val cubWriter = PrettyprintingManager.getPrinter(CUDA_Compat.defCubHeader)

      if (Knowledge.hip_enabled) {
        cubWriter.addExternalDependency("hipcub/hipcub.hpp")
        cubWriter <<< "namespace cub = hipcub;"
      } else {
        cubWriter.addExternalDependency("cub/cub.cuh")
      }
    }
  }
}
