package exastencils.knowledge

object Hardware {
  
  // TODO: check which flags are required
  var compiler = "g++"
  var cflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"
  var ldflags = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"

  var compiler_mpi = "mpixlcxx"
  var cflags_mpi = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"
  var ldflags_mpi = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG"

  var compiler_openmp = "mpixlcxx_r"
  var cflags_openmp = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG -qsmp=omp"
  var ldflags_openmp = "-O3 -qhot -qarch=qp -qtune=qp -DNDEBUG -qsmp=omp"
}