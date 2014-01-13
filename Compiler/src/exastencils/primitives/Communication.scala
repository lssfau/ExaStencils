package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.mpi._
import exastencils.omp._

case class LocalSend(var field : Field, var level : Integer, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = LocalSend\n";

  def expand(collector : StackCollector) : LoopOverFragments = {
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

  def expand(collector : StackCollector) : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer;

    // FIXME: make the next line of code more readable and robust
    val globals : Globals = StateManager.root.asInstanceOf[Root].nodes.find(node => node.isInstanceOf[Globals]).get.asInstanceOf[Globals];

    for (neigh <- neighbors) {
      if (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2)) {
        val mpiTypeName = s"mpiType_Send_${field.codeName}_${level}_${neigh._1.index}";
        var typeAlreadyExists : Boolean = (globals.variables.count(v => mpiTypeName == v.variable.name) > 0);

        if (!typeAlreadyExists) {
          globals.variables += new VariableDeclarationStatement(new Variable(s"MPI_Datatype", mpiTypeName));

          if (neigh._2.begin(1) == neigh._2.end(1)) {
            globals.initFunction.body += s"MPI_Type_vector(" ~
              NumericLiteral(neigh._2.end(2) - neigh._2.begin(2) + 1) ~ ", " ~
              NumericLiteral(neigh._2.end(0) - neigh._2.begin(0) + 1) ~ ", " ~
              NumericLiteral(Mapping.numPoints(level, 0) * Mapping.numPoints(level, 1)) ~
              s", MPI_DOUBLE, &$mpiTypeName);";
          } else if (neigh._2.begin(2) == neigh._2.end(2)) {
            globals.initFunction.body += s"MPI_Type_vector(" ~
              NumericLiteral(neigh._2.end(1) - neigh._2.begin(1) + 1) ~ ", " ~
              NumericLiteral(neigh._2.end(0) - neigh._2.begin(0) + 1) ~ ", " ~
              NumericLiteral(Mapping.numPoints(level, 0)) ~
              s", MPI_DOUBLE, &$mpiTypeName);";
          }
          globals.initFunction.body += s"MPI_Type_commit(&$mpiTypeName);";

          // TODO: free datatype          
        }

        body +=
          new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
            ListBuffer[Statement](
              new MPI_Send(
                s"&" ~ new FieldAccess(field, level, "slot", Mapping.access(level, neigh._2.begin)),
                s"1",
                mpiTypeName,
                new getNeighInfo_RemoteRank(neigh._1),
                s"((unsigned int)curFragment.id << 16) + ((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1)).cpp + ") & 0x0000ffff)",
                s"curFragment.request_Send[${neigh._1.index}]") with OMP_PotentiallyCritical,
              s"curFragment.reqOutstanding_Send[${neigh._1.index}] = true;"));
      } else {
        body +=
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
              StringLiteral(s"curFragment.reqOutstanding_Send[${neigh._1.index}] = true;")));
      }
    }

    new LoopOverFragments(body) with OMP_PotentiallyParallel;
  }
}

case class RemoteReceive(var field : Field, var level : Integer, var neighbors : ListBuffer[NeighborInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n";

  def expand(collector : StackCollector) : LoopOverFragments = {
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

  def expand(collector : StackCollector) : LoopOverFragments = {
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

  def expand(collector : StackCollector) : Statement = {
    "waitForMPICommunication();";
  }

  //  def expand(collector : StackCollector) : LoopOverFragments = {
  //    new LoopOverFragments(
  //      neighbors.map(neigh =>
  //        Array("Send", "Recv").map(sendOrRecv =>
  //          (new ConditionStatement(s"curFragment.reqOutstanding_${sendOrRecv}[${neigh.index}]",
  //            ListBuffer[Statement](
  //              s"waitForMPIReq(&curFragment.request_${sendOrRecv}[${neigh.index}]);",
  //              s"curFragment.reqOutstanding_${sendOrRecv}[${neigh.index}] = false;")) : Statement))).flatten);
  //  }
}
    