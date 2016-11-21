package exastencils.parallelization.api.mpi

import exastencils.prettyprinting.PpStream

/// MPI_Init

case object MPI_Init extends MPI_Statement {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Init(&argc, &argv);"
}

/// MPI_Finalize

case object MPI_Finalize extends MPI_Statement {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Finalize();"
}
