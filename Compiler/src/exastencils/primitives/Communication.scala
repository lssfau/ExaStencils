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
import exastencils.polyhedron._

case class CommunicateStatement(var fieldName : String, var fieldLevel : Int) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CommunicateStatement\n"

  def expand : Statement = {
    new FunctionCallExpression("exch" ~ StateManager.findFirst[FieldCollection]().get.getFieldByIdentifier(fieldName, fieldLevel).get.codeName,
      0 /* FIXME */ )
  }
}

case class LocalSend(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = LocalSend\n"

  def expand : LoopOverFragments = {
    new LoopOverFragments(field.domain,
      neighbors.map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndNotRemote(neigh._1, field.domain),
          ListBuffer[Statement](
            new LoopOverDimensions(neigh._2,
              new AssignmentStatement(
                new DirectFieldAccess(new getNeighInfo_LocalPtr(neigh._1, field.domain), field, "slot", new MultiIndex(
                  new MultiIndex(DefaultLoopMultiIndex(), neigh._3.begin, _ + _), neigh._2.begin, _ - _)),
                new DirectFieldAccess("curFragment.", field, "slot", DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))) : Statement)) with OMP_PotentiallyParallel
  }
}

case class CopyToSendBuffer(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyToSendBuffer\n"

  def expand : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer

    new LoopOverFragments(field.domain, neighbors.
      filterNot(neigh => Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))).
      filterNot(neigh => neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)).
      map(neigh =>
        new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain),
          new LoopOverDimensions(neigh._2,
            new AssignmentStatement(new ArrayAccess(s"curFragment.buffer_Send[${neigh._1.index}]", Mapping.resolveMultiIdx(new MultiIndex(DefaultLoopMultiIndex(), neigh._2.begin, _ - _), neigh._2)),
              new DirectFieldAccess("curFragment.", field, "slot", DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable) : Statement)) with OMP_PotentiallyParallel
  }
}

case class CopyFromRecvBuffer(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyFromRecvBuffer\n"

  def expand : LoopOverFragments = {
    new LoopOverFragments(field.domain, neighbors.
      filterNot(neigh => Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))).
      filterNot(neigh => neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)).
      map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain),
          new LoopOverDimensions(neigh._2,
            new AssignmentStatement(new DirectFieldAccess("curFragment.", field, "slot", DefaultLoopMultiIndex()),
              new ArrayAccess(s"curFragment.buffer_Recv[${neigh._1.index}]", Mapping.resolveMultiIdx(new MultiIndex(DefaultLoopMultiIndex(), neigh._2.begin, _ - _), neigh._2)))) with OMP_PotentiallyParallel with PolyhedronAccessable)) : Statement)) with OMP_PotentiallyParallel
  }
}

case class RemoteSend(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteSend\n"

  def addMPIDatatype(mpiTypeNameBase : String, indexRange : IndexRange) : String = {
    val globals : Globals = StateManager.findFirst[Globals]().get

    var addToName = 0
    while (globals.variables.count(v => s"${mpiTypeNameBase}_${addToName}" == v.variable.name) > 0)
      addToName += 1

    val mpiTypeName = mpiTypeNameBase + s"_${addToName}"

    globals.variables += new VariableDeclarationStatement(new VariableAccess(mpiTypeName, Some("MPI_Datatype")))

    // FIXME: this comparison doesn't work with the new MultiIndex 
    if (indexRange.begin(1) == indexRange.end(1) || indexRange.begin(2) == indexRange.end(2))
      globals.initFunction.body += InitMPIDataType(mpiTypeName, field, indexRange)

    // TODO: free datatype

    return mpiTypeName
  }

  def expand : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer

    for (neigh <- neighbors) {
      var ptr : Expression = new NullExpression
      var cnt : Expression = new NullExpression
      var typeName : Expression = new NullExpression

      // FIXME: these comparisons don't work with the new MultiIndex 
      if (neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)) {
        ptr = s"&" ~ new DirectFieldAccess("curFragment.", field, "slot", neigh._2.begin)
        cnt = 1
        typeName = s"MPI_DOUBLE"
      } else if (Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))) {
        val mpiTypeName = addMPIDatatype(s"mpiType_Send_${field.codeName.cpp}_${neigh._1.index}", neigh._2)
        ptr = s"&" ~ new DirectFieldAccess("curFragment.", field, "slot", neigh._2.begin)
        cnt = 1
        typeName = mpiTypeName
      } else {
        ptr = s"curFragment.buffer_Send[${neigh._1.index}]"
        cnt = DimArray().map(i => (neigh._2.end(i) - neigh._2.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
        typeName = s"MPI_DOUBLE"
      }

      body +=
        new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain),
          ListBuffer[Statement](
            new MPI_Send(ptr, cnt, typeName, new getNeighInfo_RemoteRank(neigh._1, field.domain),
              s"((unsigned int)curFragment.commId << 16) + ((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1, field.domain)).cpp + ") & 0x0000ffff)",
              s"curFragment.request_Send[${neigh._1.index}]") with OMP_PotentiallyCritical,
            s"curFragment.reqOutstanding_Send[${neigh._1.index}] = true"))
    }

    new LoopOverFragments(field.domain, body) with OMP_PotentiallyParallel
  }
}

case class RemoteReceive(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n"

  def addMPIDatatype(mpiTypeNameBase : String, indexRange : IndexRange) : String = {
    val globals : Globals = StateManager.findFirst[Globals]().get

    var addToName = 0
    while (globals.variables.count(v => s"${mpiTypeNameBase}_${addToName}" == v.variable.name) > 0)
      addToName += 1

    val mpiTypeName = mpiTypeNameBase + s"_${addToName}"

    globals.variables += new VariableDeclarationStatement(new VariableAccess(mpiTypeName, Some("MPI_Datatype")))

    if (indexRange.begin(1) == indexRange.end(1) || indexRange.begin(2) == indexRange.end(2))
      globals.initFunction.body += InitMPIDataType(mpiTypeName, field, indexRange)

    // TODO: free datatype

    return mpiTypeName
  }

  def expand : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer

    for (neigh <- neighbors) {
      var ptr : Expression = new NullExpression
      var cnt : Expression = new NullExpression
      var typeName : Expression = new NullExpression

      if (neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)) {
        ptr = s"&" ~ new DirectFieldAccess("curFragment.", field, "slot", neigh._2.begin)
        cnt = 1
        typeName = s"MPI_DOUBLE"
      } else if (Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))) {
        val mpiTypeName = addMPIDatatype(s"mpiType_Recv_${field.codeName.cpp}_${neigh._1.index}", neigh._2)
        ptr = s"&" ~ new DirectFieldAccess("curFragment.", field, "slot", neigh._2.begin)
        cnt = 1
        typeName = mpiTypeName
      } else {
        ptr = s"curFragment.buffer_Recv[${neigh._1.index}]"
        cnt = s"curFragment.maxElemRecvBuffer[${neigh._1.index}]"
        typeName = s"MPI_DOUBLE"
      }

      body += new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain),
        ListBuffer[Statement](
          new MPI_Receive(ptr, cnt, typeName, new getNeighInfo_RemoteRank(neigh._1, field.domain),
            s"((unsigned int)(" + (new getNeighInfo_FragmentId(neigh._1, field.domain)).cpp + ") << 16) + ((unsigned int)curFragment.commId & 0x0000ffff)",
            s"curFragment.request_Recv[${neigh._1.index}]") with OMP_PotentiallyCritical,
          s"curFragment.reqOutstanding_Recv[${neigh._1.index}] = true"))
    }

    new LoopOverFragments(field.domain, body) with OMP_PotentiallyParallel
  }
}

case class FinishRemoteSend(var neighbors : ListBuffer[NeighborInfo]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteSend\n"

  def expand : Statement = {
    "waitForMPISendOps()"
  }
}

case class FinishRemoteRecv(var neighbors : ListBuffer[NeighborInfo]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteRecv\n"

  def expand : Statement = {
    "waitForMPIRecvOps()"
  }
}
    