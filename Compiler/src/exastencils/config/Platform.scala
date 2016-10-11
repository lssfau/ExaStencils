package exastencils.config

import exastencils.constraints._
import exastencils.logger._

object Platform {
  /// target environment

  // the target operating system: "Linux", "Windows", "OSX"
  var targetOS : String = "Windows"

  // the target compiler; may atm be "MSVC", "GCC", "IBMXL", "IBMBG", "ICC", "CLANG"
  var targetCompiler : String = "MSVC"

  // major version of the target compiler
  var targetCompilerVersion : Int = 0

  // minor version of the target compiler
  var targetCompilerVersionMinor : Int = 0

  // target hw platform; may be "CPU" or "ARM"
  var targetHardware : String = "CPU"

  // target cuda compiler
  var targetCudaCompiler : String = "NVCC"

  // indicates if the compiler supports initializer lists (e.g. for std::min)
  def supports_initializerList = {
    targetCompiler match {
      case "MSVC"            => targetCompilerVersion >= 12
      case "GCC"             => targetCompilerVersion > 4 || (targetCompilerVersion == 4 && targetCompilerVersionMinor >= 5)
      case "IBMXL" | "IBMBG" => false // TODO: does it support initializer lists? since which version?
      case "ICC"             => targetCompilerVersion >= 14
      case "CLANG"           => targetCompilerVersion >= 3 // TODO: check if some minor version fails to compile
      case _                 => Logger.error("Unsupported target compiler"); false
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
      case "NEON"            => 2 * double // TODO: check if double is supported
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

  // the maximum version of omp supported by the chosen compiler
  def omp_version : Double = {
    targetCompiler match {
      case "MSVC"            => 2.0
      case "GCC"             => 4.0
      case "IBMXL" | "IBMBG" => 3.0
      case "ICC"             => if (targetCompilerVersion >= 15) 4.0; else if (targetCompilerVersion >= 13) 3.1; else if (targetCompilerVersion >= 12 && targetCompilerVersionMinor >= 1) 3.1; else 3.0
      case "CLANG"           => if (targetCompilerVersion >= 3 && targetCompilerVersionMinor >= 7) 3.1; else 0.0
      case _                 => Logger.error("Unsupported target compiler"); 0.0
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
      case _                 => Logger.error("Unsupported target compiler"); true
    }
  }

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
  var hw_gpu_name : String = "NVidia Quadro 4000"
  var hw_gpu_numDevices : Int = 2
  // in B/s
  var hw_gpu_bandwidth : Double = 89.6 * 1024 * 1024 * 1024
  // in Hz
  var hw_gpu_frequency : Double = 0.475 * 1000 * 1000 * 1000
  var hw_gpu_numCores : Int = 256
  var hw_cuda_capability : Int = 2
  var hw_cuda_capabilityMinor : Int = 0
  // amount of shared memory in byte
  var hw_cuda_sharedMemory : Int = 49152
  // cache size in byte
  var hw_cuda_cacheMemory : Int = 16384

  // 3 seems to be max; checked for versions up to 5.3
  def hw_cuda_maxNumDimsBlock : Int = if (hw_cuda_capability < 2) 2 else 3

  /// software options

  var sw_cuda_version : Int = 7
  var sw_cuda_versionMinor : Int = 5
  // in s
  var sw_cuda_kernelCallOverhead : Double = 3.5 * 0.001

  /// resolve functions

  def resolveCompiler = {
    targetCompiler match {
      case "IBMBG" =>
        val base = if (Knowledge.mpi_enabled) "mpixlcxx" else "bgxlc++"
        if (Knowledge.omp_enabled) base + "_r" else base
      case "IBMXL" =>
        if (Knowledge.mpi_enabled) "mpixlcxx" else "xlc++"
      case "GCC"   =>
        if ("ARM" == targetHardware)
          "arm-linux-gnueabihf-g++"
        else if (Knowledge.mpi_enabled)
          "mpicxx"
        else
          "g++"
      case "MSVC"  =>
        "" // nothing to do
      case "ICC"   =>
        if (Knowledge.mpi_enabled) "mpicxx" else "icpc"
      case "CLANG" =>
        "clang++-" + targetCompilerVersion + "." + targetCompilerVersionMinor
    }
  }

  def resolveCudaCompiler = {
    targetCudaCompiler match {
      case "NVCC" => "nvcc"
    }
  }

  def resolveCudaFlags = {
    var flags : String = ""

    targetCudaCompiler match {
      case "NVCC" =>
        // -arch=sm_35 tells nvcc that it is compiling for the real architecture Kepler GK110 (NVIDIA Titan Black)
        flags += " -std=c++11 -O3 -DNDEBUG -lineinfo -arch=sm_35"

        // this is required since latest Ubuntu update
        flags += " -D_MWAITXINTRIN_H_INCLUDED -D_FORCE_INLINES"
    }

    flags
  }

  def resolveCFlags = {
    var flags : String = ""

    targetCompiler match {
      case "IBMBG" | "IBMXL" =>
        flags += " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot
        if (Knowledge.omp_enabled) flags += " -qsmp=omp"
      case "GCC"             =>
        flags += " -O3 -DNDEBUG -std=c++11"

        if (Knowledge.omp_enabled) flags += " -fopenmp"

        if (Knowledge.opt_vectorize) {
          simd_instructionSet match {
            case "SSE3"   => flags += " -msse3"
            case "AVX"    => flags += " -mavx"
            case "AVX2"   => flags += " -mavx2 -mfma"
            case "AVX512" => flags += " -march=knl"
            case "IMCI"   => Logger.error("GCC does not support IMCI")
            case "NEON"   => flags += " -mfpu=neon"
          }
        }

        if ("ARM" == targetHardware) {
          flags += " -mcpu=cortex-a9 -mhard-float -funsafe-math-optimizations -static"
        }
      case "MSVC"            => // nothing to do
      case "ICC"             =>
        flags += " -O3 -std=c++11"

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
      case "CLANG"           =>
        flags += " -O3 -std=c++11"

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
    }

    flags
  }

  def resolveLdFlags = {
    var flags : String = ""

    if (Knowledge.library_CImg) {
      targetOS match {
        case "Windows"       => flags += " -lgdi32 "
        case "Linux" | "OSX" => flags += " -lm -lpthread -lX11"
      }
    }

    targetCompiler match {
      case "IBMBG" | "IBMXL" =>
        flags += " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot
        if (Knowledge.omp_enabled) flags += " -qsmp=omp"
      case "GCC"             =>
        if ("ARM" == targetHardware) flags += " -static"
        if (Knowledge.omp_enabled) flags += " -fopenmp"
      case "MSVC"            => // nothing to do
      case "ICC"             =>
        if (Knowledge.omp_enabled) {
          if (targetCompilerVersion >= 15)
            flags += " -qopenmp"
          else
            flags += " -openmp"
        }
      case "CLANG"           =>
        if (Knowledge.omp_enabled) flags += " -fopenmp=libiomp5"
    }

    flags
  }

  def update() : Unit = {
    Constraints.condEnsureValue(hw_cpu_numHWThreads, hw_cpu_numCoresPerCPU, hw_cpu_numHWThreads < hw_cpu_numCoresPerCPU, "The number of hardware threads has at least to be equal to the number of physical cores")
    Constraints.condEnsureValue(simd_mathLibrary, "none", simd_mathLibrary == "svml" && !(simd_instructionSet.startsWith("SSE") || simd_instructionSet.startsWith("AVX")), "intel svml only supports SSE* and AVX* instruction sets")
  }
}