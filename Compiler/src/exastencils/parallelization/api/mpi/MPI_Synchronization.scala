package exastencils.parallelization.api.mpi

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream

/// MPI_Barrier
case object MPI_Barrier extends MPI_Statement {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Barrier(mpiCommunicator);"
}

/// MPI_Sequential

object MPI_Sequential {
  def apply(body : IR_Statement*) = new MPI_Sequential(body.to[ListBuffer])
}

case class MPI_Sequential(var body : ListBuffer[IR_Statement]) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, "curRank", 0),
      IR_LowerExpression("curRank", Knowledge.mpi_numThreads),
      IR_PreIncrementExpression("curRank"),
      ListBuffer[IR_Statement](
        MPI_Barrier,
        IR_IfCondition(IR_EqEqExpression(MPI_IV_MpiRank, "curRank"), body)))
  }
}
