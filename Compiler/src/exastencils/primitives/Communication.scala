package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

// TODO: move to separate file for MPI statements
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

case class MPI_Send(var buffer : Expression, var size : Expression, var typeName : Expression, var rank : Expression, var tag : Expression, var request : Expression) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    // TODO: move omp stuff to separate class
    s += s"#pragma omp critical\n{\n";

    s += s"MPI_Isend(${buffer.cpp}, ${size.cpp}, ${typeName.cpp}, ${rank.cpp}, ${tag.cpp}, MPI_COMM_WORLD, &${request.cpp});\n"

    s += s"}\n";

    return s;
  }
};

case class LocalSend(var field : Field, var level : Expression, var neighbors : ListBuffer[(NeighInfo, IndexRange, IndexRange)]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = LocalSend\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      neighbors.map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndNotRemote(neigh._1),
          ListBuffer[Statement](
            s"unsigned int entry = 0;",
            new LoopOverDimensions(neigh._2,
              new AssignmentStatement(
                new LocalNeighborFieldAccess(
                  new getNeighInfo_LocalPtr(neigh._1), field, level, "slot", Mapping.access(neigh._3.level,
                    s"(z - (${neigh._2.begin(2)}) + (${neigh._3.begin(2)}))",
                    s"(y - (${neigh._2.begin(1)}) + (${neigh._3.begin(1)}))",
                    s"(x - (${neigh._2.begin(0)}) + (${neigh._3.begin(0)}))")),
                new FieldAccess(field, level, "slot", Mapping.access(neigh._2.level))))))) : Statement));
  }
}

case class CopyToSendBuffer_and_RemoteSend(var field : Field, var level : Expression /*FIXME: Int*/ , var neighbors : ListBuffer[(NeighInfo, IndexRange)]) extends Statement with Expandable {
  // FIXME: split this node
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = CopyToSendBuffer_and_RemoteSend\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      // TODO: check if a for loop could be used
      neighbors.map(neigh =>
        new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
          ListBuffer[Statement](
            s"unsigned int entry = 0;",
            new LoopOverDimensions(neigh._2,
              new AssignmentStatement(
                s"curFragment.sendBuffer_${neigh._1.label}[entry++]",
                new FieldAccess(field, level, "slot", Mapping.access(neigh._2.level)))),
            new MPI_Send(
              s"curFragment.sendBuffer_${neigh._1.label}",
              s"entry",
              s"MPI_DOUBLE",
              new getNeighInfo_RemoteRank(neigh._1),
              s"((unsigned int)curFragment.id << 16) + ((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1)).cpp + ") & 0x0000ffff)",
              s"curFragment.request_Send_${neigh._1.label}"),
            StringLiteral(s"fragments[e]->reqOutstanding_Send_${neigh._1.label} = true;"))) : Statement));
  }
}

case class RemoteReceive(var field : Field, var level : Any /*FIXME: Int*/ , var neighbors : ListBuffer[NeighInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      // TODO: check if a for loop could be used
      neighbors.map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh),
          ListBuffer[Statement](
            new MPI_Receive(
              s"curFragment.recvBuffer_${neigh.label}",
              s"curFragment.maxElemRecvBuffer_${neigh.label}",
              s"MPI_DOUBLE",
              new getNeighInfo_RemoteRank(neigh),
              s"((unsigned int)(" + (new getNeighInfo_FragmentId(neigh)).cpp + ") << 16) + ((unsigned int)curFragment.id & 0x0000ffff)",
              s"curFragment.request_Recv_${neigh.label}"),
            s"curFragment.reqOutstanding_Recv_${neigh.label} = true;"))) : Statement).to[ListBuffer]);
  }
}

case class CopyFromRecvBuffer(var field : Field, var level : Expression, var neighbors : ListBuffer[(NeighInfo, IndexRange)]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = CopyFromRecvBuffer\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      neighbors.map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
          ListBuffer[Statement](
            s"unsigned int entry = 0;",
            new LoopOverDimensions(neigh._2, 
                new AssignmentStatement(
              new FieldAccess(field, level, "slot", Mapping.access(neigh._2.level)),
                s"curFragment.recvBuffer_${neigh._1.label}[entry++];"))))) : Statement));
  }
}

case class FinishRemoteCommunication(var neighbors : ListBuffer[NeighInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteCommunication\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      neighbors.map(neigh =>
        Array("Send", "Recv").map(sendOrRecv =>
          (new ConditionStatement(s"curFragment.reqOutstanding_${sendOrRecv}_${neigh.label}",
            ListBuffer[Statement](
              s"#pragma omp critical",
              s"{",
              s"waitForMPIReq(&curFragment.request_${sendOrRecv}_${neigh.label});",
              s"}",
              s"curFragment.reqOutstanding_${sendOrRecv}_${neigh.label} = false;")) : Statement))).flatten);
  }
}
    