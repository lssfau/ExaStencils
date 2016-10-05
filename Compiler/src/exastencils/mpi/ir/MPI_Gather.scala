package exastencils.mpi.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// MPI_Gather

object MPI_Gather {
  def apply(buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression) =
    new MPI_Gather("MPI_IN_PLACE", buf, datatype, count)
}

case class MPI_Gather(var sendbuf : IR_Expression, var recvbuf : IR_Expression, var datatype : IR_Datatype, var count : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    (out << "MPI_Gather("
      << sendbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", "
      << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", "
      << 0 << ", mpiCommunicator);")
  }
}
