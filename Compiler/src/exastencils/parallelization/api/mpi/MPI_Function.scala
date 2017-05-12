package exastencils.parallelization.api.mpi

import exastencils.base.ir._

/// MPI_FunctionAccess

case class MPI_FunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess
