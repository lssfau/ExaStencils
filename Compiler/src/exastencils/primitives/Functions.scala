package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.omp._
import exastencils.polyhedron._

case class WaitForMPIRequestFunc() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIReq\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"waitForMPIReq",
      ListBuffer(VariableAccess("request", Some("MPI_Request*"))),
      ListBuffer[Statement](
        s"MPI_Status stat",
        new ConditionStatement("MPI_ERR_IN_STATUS == MPI_Wait(request, &stat)", ListBuffer[Statement](
          s"char msg[MPI_MAX_ERROR_STRING]",
          s"int len",
          s"MPI_Error_string(stat.MPI_ERROR, msg, &len)",
          "LOG_WARNING(\"MPI Error encountered (\" << msg << \")\")")),
        s"*request = MPI_Request()"))
  }
}

case class WaitForMPISendOps(var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPISendOps\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    if (Knowledge.mpi_useLoopsWherePossible) {
      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index

      new FunctionStatement(new UnitDatatype(), s"waitForMPISendOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(-1,
          new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
            new ConditionStatement(s"curFragment.reqOutstanding_Send[i]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Send[i])",
                s"curFragment.reqOutstanding_Send[i] = false")))))
    } else {
      new FunctionStatement(new UnitDatatype(), s"waitForMPISendOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(-1,
          neighbors.map(neigh =>
            new ConditionStatement(s"curFragment.reqOutstanding_Send[${neigh.index}]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Send[${neigh.index}])",
                s"curFragment.reqOutstanding_Send[${neigh.index}] = false")) : Statement)))
    }
  }
}

case class WaitForMPIRecvOps(var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIRecvOps\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    if (Knowledge.mpi_useLoopsWherePossible) {
      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index

      new FunctionStatement(new UnitDatatype(), s"waitForMPIRecvOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(-1,
          new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
            new ConditionStatement(s"curFragment.reqOutstanding_Recv[i]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Recv[i])",
                s"curFragment.reqOutstanding_Recv[i] = false")))))
    } else {
      new FunctionStatement(new UnitDatatype(), s"waitForMPIRecvOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(-1,
          neighbors.map(neigh =>
            new ConditionStatement(s"curFragment.reqOutstanding_Recv[${neigh.index}]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Recv[${neigh.index}])",
                s"curFragment.reqOutstanding_Recv[${neigh.index}] = false")) : Statement)))
    }
  }
}

case class SetIterationOffset(var location : Expression) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetIterationOffset\n"

  override def expand : SwitchStatement = {
    // FIXME: auto-generate this case using the actual neighbors
    // FIXME: THIS ONLY WORKS FOR COMM_STRAT 6
    SwitchStatement(location, ListBuffer(
      new CaseStatement(0, "iterationOffsetBegin[domain][0] = 0"),
      new CaseStatement(1, "iterationOffsetEnd[domain][0] = 0"),
      new CaseStatement(2, "iterationOffsetBegin[domain][1] = 0"),
      new CaseStatement(3, "iterationOffsetEnd[domain][1] = 0"),
      new CaseStatement(4, "iterationOffsetBegin[domain][2] = 0"),
      new CaseStatement(5, "iterationOffsetEnd[domain][2] = 0")))
  }
}

case class ConnectLocalElement() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectLocalElement\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectLocalElement",
      ListBuffer(VariableAccess("location", Some("unsigned int")), VariableAccess("fragment", Some("Fragment3DCube*")), VariableAccess("domain", Some("unsigned int"))),
      ListBuffer[Statement](
        "ASSERT_WARNING((fragment), \"Invalid fragment pointer detected\", return)",
        "Fragment3DCube& curFragment = *this", // HACK
        "int fragmentIdx = curFragment.commId", // HACK
        AssignmentStatement(FragMember_NeighborIsValid("domain", "location"), true),
        AssignmentStatement(FragMember_NeighborIsRemote("domain", "location"), false),
        AssignmentStatement(FragMember_NeighborLocalPtr("domain", "location"), "fragment"),
        AssignmentStatement(FragMember_NeighborFragCommId("domain", "location"), "fragment->commId"),
        SetIterationOffset("location")))
  }
}

case class ConnectRemoteElement() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectRemoteElement\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectRemoteElement",
      ListBuffer(VariableAccess("location", Some("unsigned int")), VariableAccess("id", Some("size_t")), VariableAccess("remoteRank", Some(IntegerDatatype())), VariableAccess("domain", Some("unsigned int"))),
      ListBuffer[Statement](
        "Fragment3DCube& curFragment = *this", // HACK
        "int fragmentIdx = curFragment.commId", // HACK
        AssignmentStatement(FragMember_NeighborIsValid("domain", "location"), true),
        AssignmentStatement(FragMember_NeighborIsRemote("domain", "location"), true),
        AssignmentStatement(FragMember_NeighborFragCommId("domain", "location"), "id"),
        AssignmentStatement(FragMember_NeighborRemoteRank("domain", "location"), "remoteRank"),
        SetIterationOffset("location")))
  }
}

case class SetupBuffers(var fields : ListBuffer[Field], var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    var body = ListBuffer[Statement]()

    for (field <- fields) {
      var numDataPoints = field.layout(0).total * field.layout(1).total * field.layout(2).total * field.dataType.resolveFlattendSize
      body += new LoopOverFragments(field.domain.index,
        //new ConditionStatement(FragMember_IsValidForSubdomain(field.domain.index),
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          new AssignmentStatement(new ArrayAccess("curFragment." ~ field.codeName, slot),
            ("new" : Expression) ~~ field.dataType.resolveUnderlyingDatatype. /*FIXME*/ cpp ~ "[" ~ numDataPoints ~ "]") : Statement)) with OMP_PotentiallyParallel
    }

    return FunctionStatement(new UnitDatatype(), s"setupBuffers", ListBuffer(), body)
  }
}

case class GetFromExternalField(var src : Field, var dest : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetFromExternalField\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), "get" ~ src.codeName,
      ListBuffer(new VariableAccess("dest", Some(PointerDatatype(src.dataType))), new VariableAccess("slot", Some(new IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => dest.layout(i).idxDupLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => dest.layout(i).idxDupRightEnd))),
          new AssignmentStatement(ExternalFieldAccess("dest", dest, DefaultLoopMultiIndex()),
            FieldAccess(FieldSelection(new NullExpression, src, "slot", -1), DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}

case class SetFromExternalField(var dest : Field, var src : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetFromExternalField\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), "set" ~ dest.codeName,
      ListBuffer(new VariableAccess("src", Some(PointerDatatype(dest.dataType))), new VariableAccess("slot", Some(new IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => src.layout(i).idxDupLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => src.layout(i).idxDupRightEnd))),
          new AssignmentStatement(FieldAccess(FieldSelection(new NullExpression, dest, "slot", -1), DefaultLoopMultiIndex()),
            ExternalFieldAccess("src", src, DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}