package exastencils.knowledge

import exastencils.logger._

object Platform {

  // TODO: check which flags are required

  var compiler : String = ""
  var cflags : String = ""
  var addcflags : String = ""
  var ldflags : String = ""
  var addldflags : String = ""

  if (Knowledge.library_CImg) {
    Knowledge.targetOS match {
      case "Windows"       => ldflags += " -lgdi32 "
      case "Linux" | "OSX" => ldflags += " -lX11 "
    }
  }

  // NOTE: this only works if the Hardware object is loaded AFTER Knowledge is fully initialized
  Knowledge.targetCompiler match {
    case "IBMBG" => {
      cflags = " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot 
      ldflags = " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot

      if (Knowledge.mpi_enabled && Knowledge.omp_enabled) {
        compiler = "mpixlcxx_r"
      } else if (Knowledge.mpi_enabled && !Knowledge.omp_enabled) {
        compiler = "mpixlcxx"
      } else if (!Knowledge.mpi_enabled && Knowledge.omp_enabled) {
        compiler = "bgxlc++_r"
      } else if (!Knowledge.mpi_enabled && !Knowledge.omp_enabled) {
        compiler = "bgxlc++"
      }

      if (Knowledge.omp_enabled) {
        cflags += " -qsmp=omp"
        ldflags += " -qsmp=omp"
      }
    }
    case "IBMXL" => {
      cflags = " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot
      ldflags = " -O3 -qarch=qp -qtune=qp -DNDEBUG" // -qhot

      if (Knowledge.mpi_enabled) {
        compiler = "mpixlcxx"
      } else {
        compiler = "xlc++"
      }

      if (Knowledge.omp_enabled) {
        cflags += " -qsmp=omp"
        ldflags += " -qsmp=omp"
      }
    }
    case "GCC" => {
      cflags = " -O3 -DNDEBUG -std=c++11"
      ldflags = ""

      if (Knowledge.opt_vectorize) {
        Knowledge.simd_instructionSet match {
          case "SSE3" => cflags += " -msse3"
          case "AVX"  => cflags += " -mavx"
          case "AVX2" => cflags += " -mavx2 -mfma"
          case "NEON" => cflags += " -mfpu=neon"
        }
      }
      if ("ARM" == Knowledge.targetHardware) {
        cflags += " -mcpu=cortex-a9 -mhard-float -funsafe-math-optimizations -static"
        ldflags += " -static"
      }

      if ("ARM" == Knowledge.targetHardware)
        compiler = "arm-linux-gnueabihf-g++"
      else if (Knowledge.mpi_enabled) {
        compiler = "mpicxx"
      } else {
        compiler = "g++"
      }

      if (Knowledge.omp_enabled) {
        cflags += " -fopenmp"
        ldflags += " -fopenmp"
      }
    }
    case "MSVC" => { /* nothing to do */ }
    case "ICC" => {
      compiler = "icc"
      cflags = " -O3 -std=c++11"

      if (Knowledge.omp_enabled) {
        cflags += " -openmp"
        ldflags += " -openmp"
      }

      if (Knowledge.opt_vectorize) {
        Knowledge.simd_instructionSet match {
          case "SSE3"   => cflags += " -mSSE3"
          case "SSSE3"  => cflags += " -mSSSE3"
          case "SSE4.1" => cflags += " -mSSE4.1"
          case "SSE4.2" => cflags += " -mSSE4.2"
          case "AVX"    => cflags += " -mAVX"
          case "AVX2"   => cflags += " -march=core-avx2"
          case _        => Logger.error("Unknown SIMD instruction set " + Knowledge.simd_instructionSet)
        }
      }
    }
    case "CLANG" => {
      compiler = "clang++-" + Knowledge.targetCompilerVersion + "." + Knowledge.targetCompilerVersionMinor
      cflags = " -O3 -std=c++11"

      if (Knowledge.omp_enabled) {
        cflags += " -fopenmp=libiomp5"
        ldflags += " -fopenmp=libiomp5"
      }

      if (Knowledge.opt_vectorize) {
        Knowledge.simd_instructionSet match {
          case "SSE3" => cflags += " -msse3"
          case "AVX"  => cflags += " -mavx"
          case "AVX2" => cflags += " -mavx2 -mfma"
        }
      }
    }
  }
}
