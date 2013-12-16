package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

case class MPI_Receive(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement with OMP_PotentiallyCritical {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def addOMPDirective : Statement /*OMP_Critical*/ = {
    (new OMP_Critical(ListBuffer[Statement](this))).cpp; // FIXME: not working without cpp as long as trafos are applied recursively by default
  }

  def cpp : String = {
    (s"MPI_Irecv(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});");
  }
};

case class MPI_Send(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement with OMP_PotentiallyCritical {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def addOMPDirective : Statement /*OMP_Critical*/ = {
    (new OMP_Critical(ListBuffer[Statement](this))).cpp; // FIXME: not working without cpp as long as trafos are applied recursively by default
  }

  def cpp : String = {
    (s"MPI_Isend(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});");
  }
};