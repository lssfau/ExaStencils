package exastencils.knowledge

import exastencils.logger._

object Platform {
  /// hardware options

  var hw_numThreadsPerNode : Int = 64 // specifies the total number of ranks (OMP and MPI) to be used when generating job scripts
  def hw_numCoresPerNode : Int = hw_cpu_numCoresPerCPU * hw_cpu_numCPUs
  var hw_numNodes : Int = 1
  var hw_cpu_name : String = "Intel Xeon E5620"
  var hw_cpu_numCoresPerCPU : Int = 4
  var hw_cpu_numCPUs : Int = 2
  var hw_cpu_bandwidth : Double = 25.6 * 1024 * 1024 * 1024 // in B/s
  var hw_cpu_frequency : Double = 2.4 * 1000 * 1000 * 1000 // in Hz
  var hw_cpu_numCyclesPerDiv : Double = 24 // arbitrary value -> to be benchmarked later
  var hw_64bit : Boolean = true // true if 64 bit addresses are used
  var hw_cacheLineSize : Int = 512 // in B
  var hw_gpu_name : String = "NVidia Quadro 4000"
  var hw_gpu_numDevices : Int = 2
  var hw_gpu_bandwidth : Double = 89.6 * 1024 * 1024 * 1024 // in B/s
  var hw_gpu_frequency : Double = 0.475 * 1000 * 1000 * 1000 // in Hz
  var hw_gpu_numCores : Int = 256
  var hw_cuda_capability : Int = 2
  var hw_cuda_capabilityMinor : Int = 0

  def hw_cuda_maxNumDimsBlock : Int = if (hw_cuda_capability < 2) 2 else 3 // 3 seems to be max; checked for versions up to 5.3

  /// software options

  var sw_cuda_version : Int = 7
  var sw_cuda_versionMinor : Int = 5
  var sw_cuda_kernelCallOverhead : Double = 3.5 * 0.001 // in s

  /// resolve functions

  def resolveCompiler = {
    Knowledge.targetCompiler match {
      case "IBMBG" =>
        var base = if (Knowledge.mpi_enabled) "mpixlcxx" else "bgxlc++"
        if (Knowledge.omp_enabled) base + "_r" else base
      case "IBMXL" =>
        if (Knowledge.mpi_enabled) "mpixlcxx" else "xlc++"
      case "GCC" =>
        if ("ARM" == Knowledge.targetHardware)
          "arm-linux-gnueabihf-g++"
        else if (Knowledge.mpi_enabled)
          "mpicxx"
        else
          "g++"
      case "MSVC" =>
        "" // nothing to do
      case "ICC" =>
        if (Knowledge.mpi_enabled) "mpicxx" else "icc"
      case "CLANG" =>
        "clang++-" + Knowledge.targetCompilerVersion + "." + Knowledge.targetCompilerVersionMinor
    }
  }

  def resolveCFlags = {
    var flags : String = ""

    Knowledge.targetCompiler match {
      case "IBMBG" | "IBMXL" =>
        flags += " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot
        if (Knowledge.omp_enabled) flags += " -qsmp=omp"
      case "GCC" =>
        flags += " -O3 -DNDEBUG -std=c++11"

        if (Knowledge.omp_enabled) flags += " -fopenmp"

        if (Knowledge.opt_vectorize) {
          Knowledge.simd_instructionSet match {
            case "SSE3"   => flags += " -msse3"
            case "AVX"    => flags += " -mavx"
            case "AVX2"   => flags += " -mavx2 -mfma"
            case "AVX512" => flags += " -march=knl"
            case "IMCI"   => Logger.error("GCC does not support IMCI")
            case "NEON"   => flags += " -mfpu=neon"
          }
        }

        if ("ARM" == Knowledge.targetHardware) {
          flags += " -mcpu=cortex-a9 -mhard-float -funsafe-math-optimizations -static"
        }
      case "MSVC" => // nothing to do
      case "ICC" =>
        flags += " -O3 -std=c++11"

        if (Knowledge.omp_enabled) flags += " -openmp"

        if (Knowledge.opt_vectorize) {
          Knowledge.simd_instructionSet match { // TODO: verify flags
            case "SSE3"   => flags += " -msse3"
            case "AVX"    => flags += " -mavx"
            case "AVX2"   => flags += " -march=core-avx2"
            case "AVX512" => flags += " -march=knl"
            case "IMCI"   => flags += " -march=knc"
          }
        }
      case "CLANG" =>
        flags += " -O3 -std=c++11"

        if (Knowledge.omp_enabled) flags += " -fopenmp=libiomp5"

        if (Knowledge.opt_vectorize) {
          Knowledge.simd_instructionSet match {
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
      Knowledge.targetOS match {
        case "Windows"       => flags += " -lgdi32 "
        case "Linux" | "OSX" => flags += " -lm -lpthread -lX11"
      }
    }

    Knowledge.targetCompiler match {
      case "IBMBG" | "IBMXL" =>
        flags += " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot
        if (Knowledge.omp_enabled) flags += " -qsmp=omp"
      case "GCC" =>
        if ("ARM" == Knowledge.targetHardware) flags += " -static"
        if (Knowledge.omp_enabled) flags += " -fopenmp"
      case "MSVC" => // nothing to do
      case "ICC" =>
        if (Knowledge.omp_enabled) flags += " -openmp"
      case "CLANG" =>
        if (Knowledge.omp_enabled) flags += " -fopenmp=libiomp5"
    }

    flags
  }
}
