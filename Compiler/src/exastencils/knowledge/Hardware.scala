package exastencils.knowledge

object Hardware {

  // TODO: check which flags are required

  var compiler : String = ""
  var cflags : String = ""
  var addcflags : String = ""
  var ldflags : String = ""
  var addldflags : String = ""

  // NOTE: this only works if the Hardware object is loaded AFTER Knowledge is fully initialized
  Knowledge.targetCompiler match {
    case "IBMXL" => {
      cflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"
      ldflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"

      if (Knowledge.useMPI && Knowledge.useOMP) {
        compiler = "mpixlcxx_r"
      } else if (Knowledge.useMPI && !Knowledge.useOMP) {
        compiler = "mpixlcxx"
      } else if (!Knowledge.useMPI && Knowledge.useOMP) {
        compiler = "xlcxx_r"
      } else if (!Knowledge.useMPI && !Knowledge.useOMP) {
        compiler = "xlcxx"
      }

      if (Knowledge.useOMP) {
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

      if (Knowledge.useMPI) {
        compiler = "mpicxx"
      } else {
        compiler = "g++"
      }

      if (Knowledge.useOMP) {
        cflags += " -fopenmp"
        ldflags += " -fopenmp"
      }
    }
    case "MSVC" => { /* nothing to do */ }
  }
}