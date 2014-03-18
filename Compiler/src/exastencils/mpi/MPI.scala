package exastencils.mpi

import scala.collection.mutable.ListBuffer
import exastencils.core.collectors._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.omp._
import exastencils.primitives._
import exastencils.knowledge._

case class MPI_IsRootProc() extends Expression {
  def cpp : String = { s"0 == mpiRank"; }
}

case class MPI_Init() extends Statement {
  def cpp : String = {
    (s"MPI_Init(&argc, &argv);");
  }
};

case class MPI_Finalize() extends Statement {
  def cpp : String = {
    (s"MPI_Finalize();");
  }
};

case class MPI_SetRankAndSize() extends Statement {
  def cpp : String = {
    (s"int mpiRank;\n"
      + s"int mpiSize;\n"
      + s"MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);\n"
      + s"MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);\n");
  }
};

case class MPI_Receive(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement {
  def cpp : String = {
    (s"MPI_Irecv(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});");
  }
};

case class MPI_Send(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement {
  def cpp : String = {
    (s"MPI_Isend(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});");
  }
};

case class MPI_Allreduce(var sendbuf : Expression, var recvbuf : Expression, var count : Expression, var op : Expression) extends Statement {
  def cpp : String = {
    // FIXME: set data-type by typed parameter
    (s"MPI_Allreduce(${sendbuf.cpp}, ${recvbuf.cpp}, ${count.cpp}, MPI_DOUBLE, ${op.cpp}, MPI_COMM_WORLD);");
  }
};

case class MPI_Barrier() extends Statement {
  def cpp : String = {
    (s"MPI_Barrier(MPI_COMM_WORLD);");
  }
};

case class InitMPIDataType(mpiTypeName : String, field : Field, indexRange : IndexRange) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = InitMPIDataType\n";

  def expand(collector : StackCollector) : StatementBlock = {
    if (indexRange.begin(2) == indexRange.end(2)) {
      return StatementBlock(ListBuffer[Statement](s"MPI_Type_vector(" ~
        (indexRange.end(1) - indexRange.begin(1) + 1) ~ ", " ~
        (indexRange.end(0) - indexRange.begin(0) + 1) ~ ", " ~
        (field.layout(0).total) ~
        s", MPI_DOUBLE, &$mpiTypeName);",
        s"MPI_Type_commit(&$mpiTypeName);"));
    } else if (indexRange.begin(1) == indexRange.end(1)) {
      return StatementBlock(ListBuffer[Statement](s"MPI_Type_vector(" ~
        (indexRange.end(2) - indexRange.begin(2) + 1) ~ ", " ~
        (indexRange.end(0) - indexRange.begin(0) + 1) ~ ", " ~
        (field.layout(0).total * field.layout(1).total) ~
        s", MPI_DOUBLE, &$mpiTypeName);",
        s"MPI_Type_commit(&$mpiTypeName);"));
    } else return StatementBlock(ListBuffer[Statement]());
  }
}


