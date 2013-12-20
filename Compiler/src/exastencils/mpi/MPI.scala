package exastencils.mpi

import scala.collection.mutable.ListBuffer
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.omp._

case class MPI_IsRootProc() extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = { s"0 == mpiRank"; }
}

case class MPI_Init() extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"MPI_Init(&argc, &argv);");
  }
};

case class MPI_Finalize() extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"MPI_Finalize();");
  }
};

case class MPI_SetRankAndSize() extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"int mpiRank;\n"
      + s"int mpiSize;\n"
      + s"MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);\n"
      + s"MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);\n");
  }
};

case class MPI_Receive(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement with OMP_PotentiallyCritical {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def addOMPDirective : OMP_Critical = {
    new OMP_Critical(this.cpp); // FIXME: remove cpp
  }

  def cpp : String = {
    (s"MPI_Irecv(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});");
  }
};

case class MPI_Send(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement with OMP_PotentiallyCritical {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def addOMPDirective : OMP_Critical = {
    new OMP_Critical(this.cpp); // FIXME: remove cpp
  }

  def cpp : String = {
    (s"MPI_Isend(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});");
  }
};

case class MPI_Allreduce(var sendbuf : Expression, var recvbuf : Expression, var count : Expression, var op : Expression) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    // FIXME: set data-type by typed parameter
    (s"MPI_Allreduce(${sendbuf.cpp}, ${recvbuf.cpp}, ${count.cpp}, MPI_DOUBLE, ${op.cpp}, MPI_COMM_WORLD);");
  }
};

case class MPI_Barrier() extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"MPI_Barrier(MPI_COMM_WORLD);");
  }
};


