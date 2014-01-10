package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.mpi._
import exastencils.omp._

case class LocalSend(var field : Field, var level : Integer, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) extends Statement with Expandable {
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
                  new getNeighInfo_LocalPtr(neigh._1), field, NumberToNumericLiteral(level), "slot", Mapping.access(level,
                    s"(x + ${neigh._3.begin(0) - neigh._2.begin(0)})",
                    s"(y + ${neigh._3.begin(1) - neigh._2.begin(1)})",
                    s"(z + ${neigh._3.begin(2) - neigh._2.begin(2)})")),
                new FieldAccess(field, level, "slot", Mapping.access(level)))) with OMP_PotentiallyParallel))) : Statement)) with OMP_PotentiallyParallel;
  }
}

case class CopyToSendBuffer_and_RemoteSend(var field : Field, var level : Integer, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  // FIXME: split this node
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = CopyToSendBuffer_and_RemoteSend\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      // TODO: check if a for loop could be used
      neighbors.map(neigh =>
        //          if (neigh._2.begin(1) == neigh._2.end(1)) {
        //            ListBuffer[Statement](
        //              s"static bool init = false;",
        //              s"static MPI_Datatype blockType;",
        //              /*new OMP_Critical(ListBuffer[Statement](*/
        //                s"if (!init) {",
        //                s"init = true;",
        //                s"MPI_Type_vector(" ~ NumericLiteral(neigh._2.end(2) - neigh._2.begin(2) + 1) ~ ", " ~ NumericLiteral(neigh._2.end(0) - neigh._2.begin(0) + 1) ~ ", " ~ NumericLiteral(Mapping.numPoints(level, 0) * Mapping.numPoints(level, 1)) ~ ", MPI_DOUBLE, &blockType);",
        //                s"MPI_Type_commit(&blockType);",
        //                s"}"/*))*/,
        //              new MPI_Send(
        //                s"&" ~ new FieldAccess(field, level, "slot", Mapping.access(level, neigh._2.begin)),
        //                s"1",
        //                s"blockType",
        //                new getNeighInfo_RemoteRank(neigh._1),
        //                s"((unsigned int)curFragment.id << 16) + ((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1)).cpp + ") & 0x0000ffff)",
        //                s"curFragment.request_Send[${neigh._1.index}]"),
        //              s"curFragment.reqOutstanding_Send[${neigh._1.index}] = true;" //new OMP_Critical(s"MPI_Type_free(&blockType);")
        //              )
        //          } else if (neigh._2.begin(2) == neigh._2.end(2)) {
        //            ListBuffer[Statement](
        //              s"static bool init = false;",
        //              s"static MPI_Datatype blockType;",
        //              /*new OMP_Critical(ListBuffer[Statement](*/
        //                s"if (!init) {",
        //                s"init = true;",
        //                s"MPI_Type_vector("~ NumericLiteral(neigh._2.end(1) - neigh._2.begin(1) + 1) ~", "~ NumericLiteral(neigh._2.end(0) - neigh._2.begin(0) + 1) ~", "~ NumericLiteral(Mapping.numPoints(level, 0)) ~", MPI_DOUBLE, &blockType);",
        //                s"MPI_Type_commit(&blockType);",
        //                s"}"/*))*/,
        //              new MPI_Send(
        //                s"&" ~ new FieldAccess(field, level, "slot", Mapping.access(level, neigh._2.begin)),
        //                s"1",
        //                s"blockType",
        //                new getNeighInfo_RemoteRank(neigh._1),
        //                s"((unsigned int)curFragment.id << 16) + ((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1)).cpp + ") & 0x0000ffff)",
        //                s"curFragment.request_Send[${neigh._1.index}]"),
        //              s"curFragment.reqOutstanding_Send[${neigh._1.index}] = true;" //new OMP_Critical(s"MPI_Type_free(&blockType);")
        //              )
        //          } else
        {
          new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
            ListBuffer[Statement](
              s"unsigned int entry = 0;",
              new LoopOverDimensions(neigh._2,
                new AssignmentStatement(
                  s"curFragment.buffer_Send[${neigh._1.index}][entry++]",
                  new FieldAccess(field, level, "slot", Mapping.access(level)))),
              new MPI_Send(
                s"curFragment.buffer_Send[${neigh._1.index}]",
                s"entry",
                s"MPI_DOUBLE",
                new getNeighInfo_RemoteRank(neigh._1),
                s"((unsigned int)curFragment.id << 16) + ((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1)).cpp + ") & 0x0000ffff)",
                s"curFragment.request_Send[${neigh._1.index}]") with OMP_PotentiallyCritical,
              StringLiteral(s"curFragment.reqOutstanding_Send[${neigh._1.index}] = true;")))
        } : Statement)) with OMP_PotentiallyParallel;
  }
}

case class RemoteReceive(var field : Field, var level : Integer, var neighbors : ListBuffer[NeighborInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      // TODO: check if a for loop could be used
      neighbors.map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh),
          ListBuffer[Statement](
            new MPI_Receive(
              s"curFragment.buffer_Recv[${neigh.index}]",
              s"curFragment.maxElemRecvBuffer[${neigh.index}]",
              s"MPI_DOUBLE",
              new getNeighInfo_RemoteRank(neigh),
              s"((unsigned int)(" + (new getNeighInfo_FragmentId(neigh)).cpp + ") << 16) + ((unsigned int)curFragment.id & 0x0000ffff)",
              s"curFragment.request_Recv[${neigh.index}]") with OMP_PotentiallyCritical,
            s"curFragment.reqOutstanding_Recv[${neigh.index}] = true;"))) : Statement).to[ListBuffer]) with OMP_PotentiallyParallel;
  }
}

case class CopyFromRecvBuffer(var field : Field, var level : Integer, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
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
                new FieldAccess(field, level, "slot", Mapping.access(level)),
                s"curFragment.buffer_Recv[${neigh._1.index}][entry++];"))))) : Statement)) with OMP_PotentiallyParallel;
  }
}

case class FinishRemoteCommunication(var neighbors : ListBuffer[NeighborInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteCommunication\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      neighbors.map(neigh =>
        Array("Send", "Recv").map(sendOrRecv =>
          (new ConditionStatement(s"curFragment.reqOutstanding_${sendOrRecv}[${neigh.index}]",
            ListBuffer[Statement](
              s"#pragma omp critical",
              s"{",
              s"waitForMPIReq(&curFragment.request_${sendOrRecv}[${neigh.index}]);",
              s"}",
              s"curFragment.reqOutstanding_${sendOrRecv}[${neigh.index}] = false;")) : Statement))).flatten) with OMP_PotentiallyParallel;
  }
}
    