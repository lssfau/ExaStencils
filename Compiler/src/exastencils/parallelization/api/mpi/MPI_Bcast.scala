package exastencils.parallelization.api.mpi

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting.PpStream

/// MPI_Broadcast

case class MPI_Bcast(var buffer : IR_Expression, var size : IR_Expression, var datatype : IR_Datatype, var root : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Bcast(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << root << ", mpiCommunicator);"
  }
}
