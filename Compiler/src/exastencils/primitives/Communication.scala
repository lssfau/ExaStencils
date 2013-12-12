package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class MPI_Receive(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    // TODO: move omp stuff to separate class
    s += s"#pragma omp critical\n{\n";

    s += s"MPI_Irecv(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});\n"

    s += s"}\n";

    return s;
  }
};

case class RemoteReceive(var field : Field, var level : Any /*FIXME: Int*/ , var neighbors : ListBuffer[NeighInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      // TODO: use for loop instead of 26 cases
      neighbors.map(neigh =>
        (new ifCond(new getNeighInfo_IsValid(neigh),
          (new ifCond(new getNeighInfo_IsRemote(neigh),
            ListBuffer[Statement](
              new MPI_Receive(
                s"curFragment.recvBuffer_${neigh.label}",
                s"curFragment.maxElemRecvBuffer_${neigh.label}",
                s"MPI_DOUBLE",
                new getNeighInfo_RemoteRank(neigh),
                s"((unsigned int)(" + (new getNeighInfo_FragmentId(neigh)).cpp + ") << 16) + ((unsigned int)curFragment.id & 0x0000ffff)",
                s"curFragment.request_Recv_${neigh.label}"),
              s"curFragment.reqOutstanding_Recv_${neigh.label} = true;"))))) : Statement).to[ListBuffer]);
  }
}

case class FinishRemoteCommunication(var neighbors : ListBuffer[NeighInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      neighbors.map(neigh =>
        Array("Send", "Recv").map(sendOrRecv =>
          (new ifCond(s"curFragment.reqOutstanding_${sendOrRecv}_${neigh.label}",
            ListBuffer[Statement](
              s"#pragma omp critical",
              s"{",
              s"waitForMPIReq(&curFragment.request_${sendOrRecv}_${neigh.label});",
              s"}",
              s"curFragment.reqOutstanding_${sendOrRecv}_${neigh.label} = false;")) : Statement))).flatten);
  }
}
