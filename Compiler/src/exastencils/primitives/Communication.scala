package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.omp._

case class LocalSend(var field : Field, var level : Int, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) extends Statement with Expandable {
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
                  new getNeighInfo_LocalPtr(neigh._1), field, level, "slot", new MultiIndex(
                    StringLiteral("x") + neigh._3.begin(0) - neigh._2.begin(0),
                    StringLiteral("y") + neigh._3.begin(1) - neigh._2.begin(1),
                    StringLiteral("z") + neigh._3.begin(2) - neigh._2.begin(2))),
                new FieldAccess(field, "slot", DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel))) : Statement)) with OMP_PotentiallyParallel;
  }
}

case class CopyToSendBuffer(var field : Field, var level : Int, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyToSendBuffer\n";

  def expand(collector : StackCollector) : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer;

    new LoopOverFragments(neighbors.
      filterNot(neigh => Knowledge.comm_useMPIDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))).
      filterNot(neigh => neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)).
      map(neigh =>
        new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
          ListBuffer[Statement](
            s"unsigned int entry = 0;",
            new LoopOverDimensions(neigh._2,
              new AssignmentStatement(s"curFragment.buffer_Send[${neigh._1.index}][entry++]",
                new FieldAccess(field, "slot", DefaultLoopMultiIndex()))))) : Statement)) with OMP_PotentiallyParallel;
  }
}

case class CopyFromRecvBuffer(var field : Field, var level : Int, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyFromRecvBuffer\n";

  def expand(collector : StackCollector) : LoopOverFragments = {
    new LoopOverFragments(neighbors.
      filterNot(neigh => Knowledge.comm_useMPIDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))).
      filterNot(neigh => neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)).
      map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
          ListBuffer[Statement](
            s"unsigned int entry = 0;",
            new LoopOverDimensions(neigh._2,
              new AssignmentStatement(new FieldAccess(field, "slot", DefaultLoopMultiIndex()),
                s"curFragment.buffer_Recv[${neigh._1.index}][entry++];"))))) : Statement)) with OMP_PotentiallyParallel;
  }
}

case class RemoteSend(var field : Field, var level : Int, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteSend\n";

  def addMPIDatatype(mpiTypeNameBase : String, indexRange : IndexRange) : String = {
    // FIXME: make the next line of code more readable and robust
    val globals : Globals = StateManager.root.asInstanceOf[Root].nodes.find(node => node.isInstanceOf[Globals]).get.asInstanceOf[Globals];

    var addToName = 0;
    while (globals.variables.count(v => s"${mpiTypeNameBase}_${addToName}" == v.variable.name) > 0)
      addToName += 1;

    val mpiTypeName = mpiTypeNameBase + s"_${addToName}";

    globals.variables += new VariableDeclarationStatement(new VariableAccess(mpiTypeName, Some("MPI_Datatype")));

    // FIXME: this comparison doesn't work with the new MultiIndex 
    if (indexRange.begin(1) == indexRange.end(1) || indexRange.begin(2) == indexRange.end(2))
      globals.initFunction.body += InitMPIDataType(mpiTypeName, indexRange, level);

    // TODO: free datatype

    return mpiTypeName;
  }

  def expand(collector : StackCollector) : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer;

    for (neigh <- neighbors) {
      var ptr : Expression = new NullExpression;
      var cnt : Expression = new NullExpression;
      var typeName : Expression = new NullExpression;

      // FIXME: these comparisons don't work with the new MultiIndex 
      if (neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)) {
        ptr = s"&" ~ new FieldAccess(field, "slot", neigh._2.begin);
        cnt = 1;
        typeName = s"MPI_DOUBLE";
      } else if (Knowledge.comm_useMPIDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))) {
        val mpiTypeName = addMPIDatatype(s"mpiType_Send_${field.codeName}_${level}_${neigh._1.index}", neigh._2);
        ptr = s"&" ~ new FieldAccess(field, "slot", neigh._2.begin);
        cnt = 1;
        typeName = mpiTypeName;
      } else {
        ptr = s"curFragment.buffer_Send[${neigh._1.index}]";
        cnt = (neigh._2.end(0) - neigh._2.begin(0) + 1) * (neigh._2.end(1) - neigh._2.begin(1) + 1) * (neigh._2.end(2) - neigh._2.begin(2) + 1);
        typeName = s"MPI_DOUBLE";
      }

      body +=
        new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
          ListBuffer[Statement](
            new MPI_Send(ptr, cnt, typeName, new getNeighInfo_RemoteRank(neigh._1),
              s"((unsigned int)curFragment.id << 16) + ((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1)).cpp + ") & 0x0000ffff)",
              s"curFragment.request_Send[${neigh._1.index}]") /*with OMP_PotentiallyCritical*/ ,
            s"curFragment.reqOutstanding_Send[${neigh._1.index}] = true;"));
    }

    new LoopOverFragments(body) /*with OMP_PotentiallyParallel*/ ;
  }
}

case class RemoteReceive(var field : Field, var level : Int, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n";

  def addMPIDatatype(mpiTypeNameBase : String, indexRange : IndexRange) : String = {
    // FIXME: make the next line of code more readable and robust
    val globals : Globals = StateManager.root.asInstanceOf[Root].nodes.find(node => node.isInstanceOf[Globals]).get.asInstanceOf[Globals];

    var addToName = 0;
    while (globals.variables.count(v => s"${mpiTypeNameBase}_${addToName}" == v.variable.name) > 0)
      addToName += 1;

    val mpiTypeName = mpiTypeNameBase + s"_${addToName}";

    globals.variables += new VariableDeclarationStatement(new VariableAccess(mpiTypeName, Some("MPI_Datatype")));

    if (indexRange.begin(1) == indexRange.end(1) || indexRange.begin(2) == indexRange.end(2))
      globals.initFunction.body += InitMPIDataType(mpiTypeName, indexRange, level);

    // TODO: free datatype

    return mpiTypeName;
  }

  def expand(collector : StackCollector) : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer;

    for (neigh <- neighbors) {
      var ptr : Expression = new NullExpression;
      var cnt : Expression = new NullExpression;
      var typeName : Expression = new NullExpression;

      if (neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)) {
        ptr = s"&" ~ new FieldAccess(field, "slot", neigh._2.begin);
        cnt = 1;
        typeName = s"MPI_DOUBLE";
      } else if (Knowledge.comm_useMPIDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))) {
        val mpiTypeName = addMPIDatatype(s"mpiType_Recv_${field.codeName}_${level}_${neigh._1.index}", neigh._2);
        ptr = s"&" ~ new FieldAccess(field, "slot", neigh._2.begin);
        cnt = 1;
        typeName = mpiTypeName;
      } else {
        ptr = s"curFragment.buffer_Recv[${neigh._1.index}]";
        cnt = s"curFragment.maxElemRecvBuffer[${neigh._1.index}]";
        typeName = s"MPI_DOUBLE";
      }

      body += new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1),
        ListBuffer[Statement](
          new MPI_Receive(ptr, cnt, typeName, new getNeighInfo_RemoteRank(neigh._1),
            s"((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1)).cpp + ") << 16) + ((unsigned int)curFragment.id & 0x0000ffff)",
            s"curFragment.request_Recv[${neigh._1.index}]") /*with OMP_PotentiallyCritical*/ ,
          s"curFragment.reqOutstanding_Recv[${neigh._1.index}] = true;"));
    }

    new LoopOverFragments(body) /*with OMP_PotentiallyParallel*/ ;
  }
}

case class FinishRemoteSend(var neighbors : ListBuffer[NeighborInfo]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteSend\n";

  def expand(collector : StackCollector) : Statement = {
    "waitForMPISendOps();";
  }
}

case class FinishRemoteRecv(var neighbors : ListBuffer[NeighborInfo]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteRecv\n";

  def expand(collector : StackCollector) : Statement = {
    "waitForMPIRecvOps();";
  }
}
    