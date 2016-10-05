package exastencils.mpi.ir

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// MPI_Receive

case class MPI_Receive(var buffer : IR_Expression, var size : IR_Expression, var datatype : IR_Datatype, var rank : IR_Expression, var tag : IR_Expression, var request : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Irecv(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", mpiCommunicator, &" << request << ");"
  }
}

/// MPI_Send

case class MPI_Send(var buffer : IR_Expression, var size : IR_Expression, var datatype : IR_Datatype, var rank : IR_Expression, var tag : IR_Expression, var request : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Isend(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", mpiCommunicator, &" << request << ");"
  }
}

