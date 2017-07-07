package exastencils.parallelization.api.mpi

import exastencils.base.ir._

/// MPI_FunctionReference

case class MPI_FunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference
