package exastencils.knowledge

object Platform {

  // TODO: check which flags are required

  var compiler : String = ""
  var cflags : String = ""
  var addcflags : String = ""
  var ldflags : String = ""
  var addldflags : String = ""

  // NOTE: this only works if the Hardware object is loaded AFTER Knowledge is fully initialized
  Knowledge.targetCompiler match {
    case "IBMBG" => {
      cflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"
      ldflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"

      if (Knowledge.mpi_enabled && Knowledge.omp_enabled) {
        compiler = "mpixlc++_r"
      } else if (Knowledge.mpi_enabled && !Knowledge.omp_enabled) {
        compiler = "mpixlc++"
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
      cflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"
      ldflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"

      if (Knowledge.mpi_enabled) {
        compiler = "mpixlc++"
      } else {
        compiler = "xlc++"
      }

      if (Knowledge.omp_enabled) {
        cflags += " -qsmp=omp"
        ldflags += " -qsmp=omp"
      }
    }
    case "GCC" => {
      cflags = "-O3 -DNDEBUG -std=c++11"
      if (Knowledge.opt_vectorize)
        Knowledge.simd_instructionSet match {
          case "SSE3" => cflags += " -msse3"
          case "AVX"  => cflags += " -mavx"
          case "AVX2" => cflags += " -mavx2 -mfma"
        }
      ldflags = ""

      if (Knowledge.mpi_enabled) {
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
  }
}