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
import exastencils.strategies.SimplifyStrategy

case class CommunicateStatement(var field : FieldSelection) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CommunicateStatement\n"

  def expand : Statement = {
    new FunctionCallExpression("exch" ~ field.codeName, field.slot)
  }
}

case class LocalSend(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = LocalSend\n"

  def expand : LoopOverFragments = {
    new LoopOverFragments(field.domain.index,
      neighbors.map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndNotRemote(neigh._1, field.domain.index),
          ListBuffer[Statement](
            new LoopOverDimensions(Knowledge.dimensionality + 1,
              neigh._2,
              new AssignmentStatement(
                new DirectFieldAccess(FieldSelection(new getNeighInfo_LocalPtr(neigh._1, field.domain.index) ~ "->", field, "slot", -1), new MultiIndex(
                  new MultiIndex(DefaultLoopMultiIndex(), neigh._3.begin, _ + _), neigh._2.begin, _ - _)),
                new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", -1), DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))) : Statement)) with OMP_PotentiallyParallel
  }
}

case class CopyToSendBuffer(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyToSendBuffer\n"

  def expand : LoopOverFragments = {
    // TODO: check if a for loop could be used
    var body : ListBuffer[Statement] = new ListBuffer

    new LoopOverFragments(field.domain.index, neighbors.
      filterNot(neigh => Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))).
      filterNot(neigh => neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)).
      map(neigh =>
        new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain.index),
          new LoopOverDimensions(Knowledge.dimensionality + 1,
            neigh._2,
            new AssignmentStatement(new ArrayAccess(FragMember_TmpBuffer(field, "Send", DimArrayHigher().map(i => (neigh._2.end(i) - neigh._2.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _), neigh._1.index),
              Mapping.resolveMultiIdx(new MultiIndex(DefaultLoopMultiIndex(), neigh._2.begin, _ - _), neigh._2)),
              new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", -1), DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable) : Statement)) with OMP_PotentiallyParallel
  }
}

case class CopyFromRecvBuffer(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyFromRecvBuffer\n"

  def expand : LoopOverFragments = {
    new LoopOverFragments(field.domain.index, neighbors.
      filterNot(neigh => Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))).
      filterNot(neigh => neigh._2.begin(0) == neigh._2.end(0) && neigh._2.begin(1) == neigh._2.end(1) && neigh._2.begin(2) == neigh._2.end(2)).
      map(neigh =>
        (new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain.index),
          new LoopOverDimensions(Knowledge.dimensionality + 1,
            neigh._2,
            new AssignmentStatement(new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", -1), DefaultLoopMultiIndex()),
              new ArrayAccess(FragMember_TmpBuffer(field, "Recv", DimArrayHigher().map(i => (neigh._2.end(i) - neigh._2.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _), neigh._1.index),
                Mapping.resolveMultiIdx(new MultiIndex(DefaultLoopMultiIndex(), neigh._2.begin, _ - _), neigh._2)))) with OMP_PotentiallyParallel with PolyhedronAccessable)) : Statement)) with OMP_PotentiallyParallel
  }
}

case class RemoteSend(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteSend\n"

  def addMPIDatatype(mpiTypeNameBase : String, indexRange : IndexRange) : String = {
    val globals : Globals = StateManager.findFirst[Globals]().get

    var addToName = 0
    while (globals.variables.count(v => s"${mpiTypeNameBase}_${addToName}" == v.name) > 0)
      addToName += 1

    val mpiTypeName = mpiTypeNameBase + s"_${addToName}"

    globals.variables += new VariableDeclarationStatement("MPI_Datatype", mpiTypeName)

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
        ptr = s"&" ~ new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", -1), neigh._2.begin)
        cnt = 1
        typeName = s"MPI_DOUBLE"
      } else if (Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))) {
        val mpiTypeName = addMPIDatatype(s"mpiType_Send_${field.codeName}_${neigh._1.index}", neigh._2)
        ptr = s"&" ~ new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", -1), neigh._2.begin)
        cnt = 1
        typeName = mpiTypeName
      } else {
        cnt = DimArrayHigher().map(i => (neigh._2.end(i) - neigh._2.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
        ptr = FragMember_TmpBuffer(field, "Send", cnt, neigh._1.index)
        typeName = s"MPI_DOUBLE"
      }

      SimplifyStrategy.doUntilDoneStandalone(cnt)

      body +=
        new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain.index),
          ListBuffer[Statement](
            new MPI_Send(ptr, cnt, typeName, new getNeighInfo_RemoteRank(neigh._1, field.domain.index),
              s"((unsigned int)curFragment.commId << 16) + ((unsigned int)(" ~ getNeighInfo_FragmentId(neigh._1, field.domain.index) ~ ") & 0x0000ffff)",
              FragMember_MpiRequest(field, "Send", neigh._1.index)) with OMP_PotentiallyCritical,
            AssignmentStatement(FragMember_ReqOutstanding(field, "Send", neigh._1.index), true)))
    }

    new LoopOverFragments(field.domain.index, body) with OMP_PotentiallyParallel
  }
}

case class RemoteReceive(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n"

  def addMPIDatatype(mpiTypeNameBase : String, indexRange : IndexRange) : String = {
    val globals : Globals = StateManager.findFirst[Globals]().get

    var addToName = 0
    while (globals.variables.count(v => s"${mpiTypeNameBase}_${addToName}" == v.name) > 0)
      addToName += 1

    val mpiTypeName = mpiTypeNameBase + s"_${addToName}"

    globals.variables += new VariableDeclarationStatement("MPI_Datatype", mpiTypeName)

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
        ptr = s"&" ~ new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", -1), neigh._2.begin)
        cnt = 1
        typeName = s"MPI_DOUBLE"
      } else if (Knowledge.mpi_useCustomDatatypes && (neigh._2.begin(1) == neigh._2.end(1) || neigh._2.begin(2) == neigh._2.end(2))) {
        val mpiTypeName = addMPIDatatype(s"mpiType_Recv_${field.codeName}_${neigh._1.index}", neigh._2)
        ptr = s"&" ~ new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", -1), neigh._2.begin)
        cnt = 1
        typeName = mpiTypeName
      } else {
        cnt = DimArrayHigher().map(i => (neigh._2.end(i) - neigh._2.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
        ptr = FragMember_TmpBuffer(field, "Recv", cnt, neigh._1.index)
        typeName = s"MPI_DOUBLE"
      }

      SimplifyStrategy.doUntilDoneStandalone(cnt)

      body += new ConditionStatement(new getNeighInfo_IsValidAndRemote(neigh._1, field.domain.index),
        ListBuffer[Statement](
          new MPI_Receive(ptr, cnt, typeName, new getNeighInfo_RemoteRank(neigh._1, field.domain.index),
            "((unsigned int)(" ~ getNeighInfo_FragmentId(neigh._1, field.domain.index) ~ ") << 16) + ((unsigned int)curFragment.commId & 0x0000ffff)",
            FragMember_MpiRequest(field, "Recv", neigh._1.index)) with OMP_PotentiallyCritical,
          AssignmentStatement(FragMember_ReqOutstanding(field, "Recv", neigh._1.index), true)))
    }

    new LoopOverFragments(field.domain.index, body) with OMP_PotentiallyParallel
  }
}

case class FinishRemoteSend(var neighbors : ListBuffer[NeighborInfo], var field : Field) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteSend\n"

  def expand : Statement = {
    //"waitForMPISendOps()"
    if (Knowledge.mpi_useLoopsWherePossible) {
      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index

      new LoopOverFragments(-1,
        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
          new ConditionStatement(FragMember_ReqOutstanding(field, "Send", "i"),
            ListBuffer[Statement](
              WaitForMPIReq(FragMember_MpiRequest(field, "Send", "i")),
              AssignmentStatement(FragMember_ReqOutstanding(field, "Send", "i"), false)))))
    } else {
      new LoopOverFragments(-1,
        neighbors.map(neigh =>
          new ConditionStatement(FragMember_ReqOutstanding(field, "Send", neigh.index),
            ListBuffer[Statement](
              WaitForMPIReq(FragMember_MpiRequest(field, "Send", neigh.index)),
              AssignmentStatement(FragMember_ReqOutstanding(field, "Send", neigh.index), false))) : Statement))
    }
  }
}

case class FinishRemoteRecv(var neighbors : ListBuffer[NeighborInfo], var field : Field) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = FinishRemoteRecv\n"

  def expand : Statement = {
    //"waitForMPIRecvOps()"
    if (Knowledge.mpi_useLoopsWherePossible) {
      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index

      new LoopOverFragments(-1,
        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
          new ConditionStatement(FragMember_ReqOutstanding(field, "Recv", "i"),
            ListBuffer[Statement](
              WaitForMPIReq(FragMember_MpiRequest(field, "Recv", "i")),
              AssignmentStatement(FragMember_ReqOutstanding(field, "Recv", "i"), false)))))
    } else {
      new LoopOverFragments(-1,
        neighbors.map(neigh =>
          new ConditionStatement(FragMember_ReqOutstanding(field, "Recv", neigh.index),
            ListBuffer[Statement](
              WaitForMPIReq(FragMember_MpiRequest(field, "Recv", neigh.index)),
              AssignmentStatement(FragMember_ReqOutstanding(field, "Recv", neigh.index), false))) : Statement))
    }
  }
}

case class WaitForMPIReq(var request : Expression) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIReq\n"

  def expand : Statement = {
    "waitForMPIReq(&" ~ request ~ ")"
  }
}