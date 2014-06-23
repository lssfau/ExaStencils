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

//case class WaitForMPISendOps(var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
//  override def cpp : String = "NOT VALID ; CLASS = WaitForMPISendOps\n"
//  override def cpp_decl : String = cpp
//
//  override def expand : FunctionStatement = {
//    if (Knowledge.mpi_useLoopsWherePossible) {
//      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
//      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
//
//      new FunctionStatement(new UnitDatatype(), s"waitForMPISendOps", ListBuffer[VariableAccess](),
//        new LoopOverFragments(-1,
//          new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
//            new ConditionStatement(s"curFragment.reqOutstanding_Send[i]",
//              ListBuffer[Statement](
//                s"waitForMPIReq(&curFragment.request_Send[i])",
//                s"curFragment.reqOutstanding_Send[i] = false")))))
//    } else {
//      new FunctionStatement(new UnitDatatype(), s"waitForMPISendOps", ListBuffer[VariableAccess](),
//        new LoopOverFragments(-1,
//          neighbors.map(neigh =>
//            new ConditionStatement(s"curFragment.reqOutstanding_Send[${neigh.index}]",
//              ListBuffer[Statement](
//                s"waitForMPIReq(&curFragment.request_Send[${neigh.index}])",
//                s"curFragment.reqOutstanding_Send[${neigh.index}] = false")) : Statement)))
//    }
//  }
//}
//
//case class WaitForMPIRecvOps(var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
//  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIRecvOps\n"
//  override def cpp_decl : String = cpp
//
//  override def expand : FunctionStatement = {
//    if (Knowledge.mpi_useLoopsWherePossible) {
//      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
//      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
//
//      new FunctionStatement(new UnitDatatype(), s"waitForMPIRecvOps", ListBuffer[VariableAccess](),
//        new LoopOverFragments(-1,
//          new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
//            new ConditionStatement(s"curFragment.reqOutstanding_Recv[i]",
//              ListBuffer[Statement](
//                s"waitForMPIReq(&curFragment.request_Recv[i])",
//                s"curFragment.reqOutstanding_Recv[i] = false")))))
//    } else {
//      new FunctionStatement(new UnitDatatype(), s"waitForMPIRecvOps", ListBuffer[VariableAccess](),
//        new LoopOverFragments(-1,
//          neighbors.map(neigh =>
//            new ConditionStatement(s"curFragment.reqOutstanding_Recv[${neigh.index}]",
//              ListBuffer[Statement](
//                s"waitForMPIReq(&curFragment.request_Recv[${neigh.index}])",
//                s"curFragment.reqOutstanding_Recv[${neigh.index}] = false")) : Statement)))
//    }
//  }
//}

case class SetIterationOffset(var location : Expression, var domain : Expression, var fragment : Expression) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetIterationOffset\n"

  override def expand : SwitchStatement = {
    var cases : ListBuffer[CaseStatement] = ListBuffer()

    for (neigh <- Fragment.neighbors) {
      // neighbor directions are always 3D vectors; invalid directions are not part of the given collection
      neigh.dir match {
        case Array(-1, 0, 0) => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 0), 0))
        case Array(1, 0, 0)  => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 0), 0))
        case Array(0, -1, 0) => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 1), 0))
        case Array(0, 1, 0)  => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 1), 0))
        case Array(0, 0, -1) => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 2), 0))
        case Array(0, 0, 1)  => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 2), 0))
      }
    }

    SwitchStatement(location, cases)
  }
}

case class ConnectLocalElement() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectLocalElement\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectLocalElement",
      ListBuffer(
        VariableAccess("localFragId", Some("size_t")),
        VariableAccess("localNeighId", Some("size_t")),
        VariableAccess("location", Some(IntegerDatatype())),
        VariableAccess("domain", Some(IntegerDatatype()))),
      ListBuffer[Statement](
        AssignmentStatement(iv.NeighborIsValid("domain", "location", "localFragId"), true),
        AssignmentStatement(iv.NeighborIsRemote("domain", "location", "localFragId"), false),
        AssignmentStatement(iv.NeighborFragLocalId("domain", "location", "localFragId"), "localNeighId"),
        SetIterationOffset("location", "domain", "localFragId")))
  }
}

case class ConnectRemoteElement() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectRemoteElement\n"
  override def cpp_decl : String = cpp

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectRemoteElement",
      ListBuffer(
        VariableAccess("localFragId", Some("size_t")),
        VariableAccess("localNeighId", Some("size_t")),
        VariableAccess("remoteRank", Some(IntegerDatatype())),
        VariableAccess("location", Some(IntegerDatatype())),
        VariableAccess("domain", Some(IntegerDatatype()))),
      ListBuffer[Statement](
        AssignmentStatement(iv.NeighborIsValid("domain", "location", "localFragId"), true),
        AssignmentStatement(iv.NeighborIsRemote("domain", "location", "localFragId"), true),
        AssignmentStatement(iv.NeighborFragLocalId("domain", "location", "localFragId"), "localNeighId"),
        AssignmentStatement(iv.NeighborRemoteRank("domain", "location", "localFragId"), "remoteRank"),
        SetIterationOffset("location", "domain", "localFragId")))
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
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          new AssignmentStatement(new iv.FieldData(field, slot),
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
            FieldAccess(FieldSelection(src, "slot", -1), DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))
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
          new AssignmentStatement(FieldAccess(FieldSelection(dest, "slot", -1), DefaultLoopMultiIndex()),
            ExternalFieldAccess("src", src, DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}