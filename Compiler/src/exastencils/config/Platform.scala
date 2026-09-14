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

package exastencils.config

import exastencils.constraints._
import exastencils.core.LegacyParameters
import exastencils.logger._

object Platform extends LegacyParameters {
  // === Legacy Parameters ===

  /// hardware parameters that were named *cuda* before backends other than CUDA were supported
  private val legacyGpuParameters = List(
    "sharedMemory", "cacheMemory", "warpSize", "maxNumThreads",
    "maxBlockSize_x", "maxBlockSize_y", "maxBlockSize_z")

  override val legacyParameters : Map[String, String] =
    legacyGpuParameters.map(p => s"hw_cuda_$p" -> s"hw_gpu_$p").toMap +
      ("targetCudaCompiler" -> "targetGpuCompiler")

  /// target environment

  // name of the target system; e.g. name of the cluster
  var targetName : String = "i10staff40"

  // the target operating system: "Linux", "Windows", "macOS"
  var targetOS : String = "Windows"

  // optionally set name to compiler binary to be used in Makefile
  var targetCompilerBinary : String = ""

  // the target compiler; may atm be "MSVC", "GCC", "IBMXL", "IBMBG", "ICC", "CLANG", "CRAY", "PGI"
  var targetCompiler : String = "GCC"

  // major version of the target compiler
  var targetCompilerVersion : Int = 8

  // minor version of the target compiler
  var targetCompilerVersionMinor : Int = 2

  // target hw platform; may be "CPU" or "ARM"
  var targetHardware : String = "CPU"

  // device compiler used for the selected GPU backend; "NVCC" for CUDA, "HIPCC" for HIP
  var targetGpuCompiler : String = "NVCC"

  // selected GPU backend; "CUDA" or "HIP" - see Knowledge.cuda_enabled / Knowledge.hip_enabled
  def gpuBackend : String = if (Knowledge.hip_enabled) "HIP" else "CUDA"

  // architecture the device code is compiled for
  // offloadArch for HIP (AMD), compute capability for CUDA (NVIDIA)
  def gpuArchitecture : String =
    if (Knowledge.hip_enabled) hw_hip_offloadArch
    else s"$hw_cuda_capability$hw_cuda_capabilityMinor"

  // file extension of the generated device code files; recognized by both the device compilers and CMake
  def gpuFileExtension : String = if (Knowledge.hip_enabled) "hip" else "cu"

  // guard for code which may only be seen by the device compiler of the selected GPU backend
  def gpuDeviceCompilerGuard : String = "#if defined(__CUDACC__) || defined(__HIPCC__)"

  // indicates if the compiler supports initializer lists (e.g. for std::min)
  def supports_initializerList = {
    targetCompiler match {
      case "MSVC"            => targetCompilerVersion >= 12
      case "GCC"             => targetCompilerVersion > 4 || (targetCompilerVersion == 4 && targetCompilerVersionMinor >= 5)
      case "IBMXL" | "IBMBG" => false // TODO: does it support initializer lists? since which version?
      case "ICC"             => targetCompilerVersion >= 14
      case "CLANG"           => targetCompilerVersion >= 3 // TODO: check if some minor version fails to compile
      case "Cray"            => targetCompilerVersion > 8 || (targetCompilerVersion == 8 && targetCompilerVersionMinor >= 4)
      case "PGI"             => targetCompilerVersion >= 2015

      case _ => Logger.error("Unsupported target compiler"); false
    }
  }

  /// SIMD

  // currently allowed: "SSE3", "AVX", "AVX2", "AVX512", "IMCI", "QPX", "NEON"
  var simd_instructionSet : String = "AVX"

  // number of vector elements for SIMD instructions (currently only double precision)
  def simd_vectorSize : Int = {
    val double : Int = if (Knowledge.useDblPrecision) 1 else 2
    simd_instructionSet match {
      case "SSE3"            => 2 * double
      case "AVX" | "AVX2"    => 4 * double
      case "AVX512" | "IMCI" => 8 * double
      case "QPX"             => 4 // yes, it's always 4
      case "NEON"            => 2 * double
    }
  }

  // header for vector intrinsics
  def simd_header : String = {
    simd_instructionSet match {
      case "SSE3" | "AVX" | "AVX2" | "AVX512" | "IMCI" => "immintrin.h"
      case "NEON"                                      => "arm_neon.h"
      case "QPX"                                       => null
    }
  }

  // currently allowed: "none", "svml", "vecmathlib", "vectorclass", "mass_simd"; ensure the libraries are installed system-wide, or set Settings.{pathsInc|pathsLib} accordingly
  var simd_mathLibrary : String = "none"
  def simd_mathLibHeader : Seq[String] = {
    simd_mathLibrary match {
      case "none" | "svml" => null
      case "vectorclass"   => "vectorclass.h" :: "vectormath_exp.h" :: "vectormath_trig.h" :: "vectormath_hyp.h" :: Nil
      case _               => simd_mathLibrary + ".h" :: Nil
    }
  }

  /// OMP

  // the omp version supported by the chosen compiler
  def omp_version : Double = {
    targetCompiler match {
      case "MSVC" => 2.0

      case "GCC" =>
        targetCompilerVersion + 0.1 * targetCompilerVersionMinor match {
          case x if x >= 6.1 => 4.5
          case x if x >= 4.9 => 4.0
          case x if x >= 4.7 => 3.1
          case x if x >= 4.4 => 3.0
          case x if x >= 4.2 => 2.5
          case _             => Logger.error(s"Unsupported $targetCompiler version"); 0.0
        }

      case "IBMXL" | "IBMBG" => 3.1

      case "ICC" =>
        targetCompilerVersion + 0.1 * targetCompilerVersionMinor match {
          case x if x >= 17.0 => 4.5
          case x if x >= 15.0 => 4.0
          case x if x >= 12   => 3.0
          case _              => Logger.error(s"Unsupported $targetCompiler version"); 0.0
        }

      case "CLANG" =>
        targetCompilerVersion + 0.1 * targetCompilerVersionMinor match {
          case x if x >= 3.7 => 3.1
          case _             => Logger.error(s"Unsupported $targetCompiler version"); 0.0
        }

      case "Cray" =>
        targetCompilerVersion + 0.1 * targetCompilerVersionMinor match {
          case x if x >= 8.5 => 4.0
          case _             => Logger.error(s"Unsupported $targetCompiler version"); 0.0
        }

      case "PGI" => 3.1

      case _ => Logger.error("Unsupported target compiler"); 0.0
    }
  }

  // true if the chosen compiler / mpi version requires critical sections to be marked explicitly
  def omp_requiresCriticalSections : Boolean = {
    targetCompiler match {
      case "MSVC"            => true
      case "GCC"             => true
      case "IBMXL" | "IBMBG" => true // needs to be true since recently
      case "ICC"             => true
      case "CLANG"           => true
      case "Cray"            => true
      case "PGI"             => true
      case _                 => Logger.error("Unsupported target compiler"); true
    }
  }

  // pinning scheme for (omp) threads
  var omp_pinning : String = ""

  /// MPI

  // the target mpi implementation; may currently be "OpenMPI", "IntelMPI" or "MSMPI"
  var mpi_variant : String = "OpenMPI"

  /// hardware options

  // available resources
  var hw_numNodesAvailable : Int = 1
  // actually used resources
  var hw_numNodes : Int = 1

  // specifies the total number of ranks (OMP and MPI) to be used when generating job scripts
  var hw_numThreadsPerNode : Int = 64
  def hw_numCoresPerNode : Int = hw_cpu_numCoresPerCPU * hw_cpu_numCPUs
  def hw_numHWThreadsPerNode : Int = hw_cpu_numHWThreads * hw_cpu_numCPUs

  var hw_cpu_name : String = "Intel Xeon E5620"
  var hw_cpu_numCoresPerCPU : Int = 4
  // number of hardware threads per cpu
  var hw_cpu_numHWThreads : Int = 8
  var hw_cpu_numCPUs : Int = 2
  // in B/s
  var hw_cpu_bandwidth : Double = 25.6 * 1024 * 1024 * 1024
  // in Hz
  var hw_cpu_frequency : Double = 2.4 * 1000 * 1000 * 1000
  // arbitrary value -> to be benchmarked later
  var hw_cpu_numCyclesPerDiv : Double = 24
  // true if 64 bit addresses are used
  var hw_64bit : Boolean = true
  // in B
  var hw_cacheLineSize : Int = 512
  var hw_numCaches : Int = 1
  var hw_cacheSize : Double = 32 * 1000 * 1000
  var hw_usableCache : Double = 1.0 // as multiplicative factor
  var hw_numCacheSharingThreads : Int = 4
  var hw_gpu_name : String = "NVidia Quadro 4000"
  var hw_gpu_numDevices : Int = 2
  // in B/s
  var hw_gpu_bandwidth : Double = 89.6 * 1024 * 1024 * 1024
  // in Hz
  var hw_gpu_frequency : Double = 0.475 * 1000 * 1000 * 1000
  var hw_gpu_numCores : Int = 256
  var hw_gpu_gpuDirectAvailable : Boolean = true
  var hw_cuda_capability : Int = 2
  var hw_cuda_capabilityMinor : Int = 0
  // amount of shared memory in byte
  var hw_gpu_sharedMemory : Int = 49152
  // cache size in byte
  var hw_gpu_cacheMemory : Int = 16384
  // default warp size (32)
  var hw_gpu_warpSize : Long = 32
  // max number of threads
  var hw_gpu_maxNumThreads : Long = 1024
  // max block size per dim
  var hw_gpu_maxBlockSize_x : Long = 1024
  var hw_gpu_maxBlockSize_y : Long = 1024
  var hw_gpu_maxBlockSize_z : Long = 64
  def hw_gpu_maxBlockSizes : Array[Long] = Array(hw_gpu_maxBlockSize_x, hw_gpu_maxBlockSize_y, hw_gpu_maxBlockSize_z)

  // 3 seems to be max; checked for versions up to 5.3
  def hw_gpu_maxNumDimsBlock : Int = if (hw_cuda_capability < 2) 2 else 3

  /// legacy names of the derived hardware parameters above
  def hw_cuda_maxBlockSizes : Array[Long] = hw_gpu_maxBlockSizes
  def hw_cuda_maxNumDimsBlock : Int = hw_gpu_maxNumDimsBlock

  /// software options

  // offload architecture used for AMD devices, e.g. "gfx90a" (MI200) or "gfx942" (MI300)
  // NVIDIA devices use hw_cuda_capability instead
  var hw_hip_offloadArch : String = "gfx90a"

  var sw_cuda_version : Int = 7
  var sw_cuda_versionMinor : Int = 5
  // in s
  var sw_cuda_kernelCallOverhead : Double = 0.01 * 0.001

  /// resolve functions

  def resolveCompiler = {
    if (!targetCompilerBinary.isEmpty) {
      targetCompilerBinary
    } else {
      targetCompiler match {
        case "IBMBG" =>
          val base = if (Knowledge.mpi_enabled) "mpixlcxx" else "bgxlc++"
          if (Knowledge.omp_enabled) base + "_r" else base
        case "IBMXL" =>
          if (Knowledge.mpi_enabled) "mpixlcxx" else "xlc++"
        case "GCC"   =>
          if ("ARM" == targetHardware) {
            "aarch64-linux-gnu-g++"
          } else if ("tsubame" == targetName.toLowerCase() || "tsubame3" == targetName.toLowerCase()) {
            // special override for Tsubame
            if (!Knowledge.mpi_enabled)
              "g++"
            else mpi_variant.toLowerCase() match {
              case "openmpi" => "mpicxx"
              case other     => Logger.error(s"Unsupported mpi variant $other")
            }
          } else {
            if (Knowledge.mpi_enabled) "mpicxx" else "g++"
          }
        case "MSVC"  =>
          "" // nothing to do
        case "ICC"   =>
          if ("tsubame" == targetName.toLowerCase() || "tsubame3" == targetName.toLowerCase()) {
            // special override for Tsubame
            if (!Knowledge.mpi_enabled)
              "icpc"
            else mpi_variant.toLowerCase() match {
              case "openmpi"                          => "mpicxx"
              case "intelmpi" | "intel-mpi" | "intel" => "mpiicpc"
              case other                              => Logger.error(s"Unsupported mpi variant $other")
            }
          } else {
            if (Knowledge.mpi_enabled) "mpicxx" else "icpc"
          }
        case "CLANG" =>
          "clang++-" + targetCompilerVersion + "." + targetCompilerVersionMinor
        case "Cray"  =>
          "CC" // no special wrapper required for PizDaint -> might need to be changed later
        case "PGI"   => ???
      }
    }
  }

  def resolveGpuCompiler = {
    targetGpuCompiler match {
      case "NVCC"  => "nvcc"
      case "HIPCC" => "hipcc"
      case other   => Logger.error(s"Unsupported GPU compiler $other")
    }
  }

  def resolveGpuFlags = {
    var flags : String = ""

    targetGpuCompiler match {
      case "NVCC" =>
        flags += s" -std=c++${Settings.cxx_standard} -O3 -DNDEBUG -lineinfo -arch=sm_$gpuArchitecture"

      case "HIPCC" =>
        // hipcc does not generate position independent code by default, unlike the (default PIE) host compilers linking it
        flags += s" -std=c++${Settings.cxx_standard} -O3 -DNDEBUG --offload-arch=$gpuArchitecture -fPIE"

      case other => Logger.error(s"Unsupported GPU compiler $other")
    }

    // cannot find mpi.h from Globals/Globals.h when compiling with the device compiler otherwise
    if (Knowledge.mpi_enabled) {
      val mpiWrapperFlags = s"$$(shell $resolveCompiler --showme:compile | sed 's/-pthread//g')"
      if (!Settings.makefile_additionalGpuFlags.contains(mpiWrapperFlags))
        Settings.makefile_additionalGpuFlags += mpiWrapperFlags
    }

    flags
  }

  def resolveCFlags = {
    var flags : String = ""

    targetCompiler match {
      case "IBMBG" | "IBMXL" =>
        flags += " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot
        if (Knowledge.omp_enabled) flags += " -qsmp=omp"

      case "GCC" =>
        flags += f" -O3 -DNDEBUG -std=c++${Settings.cxx_standard}"

        if (Knowledge.omp_enabled) flags += " -fopenmp"

        if (Knowledge.opt_vectorize) {
          simd_instructionSet match {
            case "SSE3"   => flags += " -msse3"
            case "AVX"    => flags += " -mavx"
            case "AVX2"   => flags += " -mavx2 -mfma"
            case "AVX512" => flags += " -march=skylake-avx512"
            case "IMCI"   => Logger.error("GCC does not support IMCI")
            case "NEON"   => // flags += " -mfpu=neon" // neon is implied when using aarch64 g++
          }
        }

        if ("ARM" == targetHardware)
          flags += " -funsafe-math-optimizations"

      case "MSVC" => // nothing to do

      case "ICC" =>
        flags += f" -O3 -fno-alias -std=c++${Settings.cxx_standard}"

        if (Knowledge.omp_enabled) {
          if (targetCompilerVersion >= 15)
            flags += " -qopenmp"
          else
            flags += " -openmp"
        }

        if (Knowledge.opt_vectorize) {
          simd_instructionSet match {
            case "SSE3"   => flags += " -xSSE3"
            case "AVX"    => flags += " -xAVX"
            case "AVX2"   => flags += " -xCORE-AVX2"
            case "AVX512" => flags += " -xCORE-AVX512"
            case "IMCI"   => flags += " -march=knc" // TODO: verify flag
          }
        }

      case "CLANG" =>
        flags += f" -O3 -std=c++${Settings.cxx_standard} -Wno-parentheses-equality"

        if (Knowledge.omp_enabled) flags += " -fopenmp=libiomp5"

        if (Knowledge.opt_vectorize) {
          simd_instructionSet match {
            case "SSE3"   => flags += " -msse3"
            case "AVX"    => flags += " -mavx"
            case "AVX2"   => flags += " -mavx2 -mfma"
            case "AVX512" => flags += " -mavx512f"
            case "IMCI"   => Logger.error("clang does not support IMCI")
          }
        }

      case "Cray" =>
        flags += " -O3 -hstd=c++11"

        if (Knowledge.omp_enabled)
          flags += " -homp"
        else
          flags += " -hnoomp"

      case "PGI" => ???
    }

    flags
  }

  def resolveLdFlags = {
    var flags : String = ""

    targetCompiler match {
      case "IBMBG" | "IBMXL" =>
        flags += " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot
        if (Knowledge.omp_enabled) flags += " -qsmp=omp"

      case "GCC" =>
        if (Knowledge.omp_enabled) flags += " -fopenmp"

      case "MSVC" => // nothing to do

      case "ICC" =>
        if (Knowledge.omp_enabled) {
          if (targetCompilerVersion >= 15)
            flags += " -qopenmp"
          else
            flags += " -openmp"
        }

      case "CLANG" =>
        if (Knowledge.omp_enabled) flags += " -fopenmp=libiomp5"

      case "Cray" =>
        if (Knowledge.omp_enabled)
          flags += " -homp"
        else
          flags += " -hnoomp"

      case "PGI" => ???

    }

    flags
  }

  def update() : Unit = {
    // keep the device compiler consistent with the GPU backend selected via Knowledge
    Constraints.condEnsureValue(targetGpuCompiler, "HIPCC", Knowledge.hip_enabled && "NVCC" == targetGpuCompiler, "the HIP backend requires a HIP capable device compiler")
    Constraints.condEnsureValue(targetGpuCompiler, "NVCC", Knowledge.cuda_enabled && "HIPCC" == targetGpuCompiler, "the CUDA backend requires a CUDA capable device compiler")

    Constraints.condEnsureValue(Settings.cxx_standard, 14, Knowledge.hip_enabled && !Knowledge.gpu_useDefaultReductions && Settings.cxx_standard < 14, "hipCUB reductions require C++14 (rocPRIM)")

    Constraints.condEnsureValue(hw_cpu_numHWThreads, hw_cpu_numCoresPerCPU, hw_cpu_numHWThreads < hw_cpu_numCoresPerCPU, "The number of hardware threads has at least to be equal to the number of physical cores")
    Constraints.condEnsureValue(simd_mathLibrary, "none", simd_mathLibrary == "svml" && !(simd_instructionSet.startsWith("SSE") || simd_instructionSet.startsWith("AVX")), "intel svml only supports SSE* and AVX* instruction sets")
  }
}
