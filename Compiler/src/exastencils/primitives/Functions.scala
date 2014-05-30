package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.omp._
import exastencils.polyhedron._

case class WaitForMPIReq() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIReq\n"

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

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectLocalElement",
      ListBuffer(VariableAccess("location", Some("unsigned int")), VariableAccess("fragment", Some("Fragment3DCube*")), VariableAccess("domain", Some("unsigned int"))),
      ListBuffer[Statement](
        "ASSERT_WARNING((fragment), \"Invalid fragment pointer detected\", return)",
        s"neighbor_isValid[domain][location] = true",
        s"neighbor_isRemote[domain][location] = false",
        s"neighbor_localPtr[domain][location] = fragment",
        s"neighbor_fragCommId[domain][location] = fragment->commId",
        SetIterationOffset("location")))
  }
}

case class ConnectRemoteElement() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectRemoteElement\n"

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectRemoteElement",
      ListBuffer(VariableAccess("location", Some("unsigned int")), VariableAccess("id", Some("size_t")), VariableAccess("remoteRank", Some(IntegerDatatype())), VariableAccess("domain", Some("unsigned int"))),
      ListBuffer[Statement](
        s"neighbor_isValid[domain][location] = true",
        s"neighbor_isRemote[domain][location] = true",
        s"neighbor_fragCommId[domain][location] = id",
        s"neighbor_remoteRank[domain][location] = remoteRank",
        SetIterationOffset("location")))
  }
}

case class SetupBuffers(var fields : ListBuffer[Field], var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n"

  override def expand : FunctionStatement = {
    var body = ListBuffer[Statement]()

    for (field <- fields) {
      body += new ConditionStatement(s"isValidForSubdomain[${field.domain}]",
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          new AssignmentStatement(field.codeName ~ "[" ~ slot ~ "]", ("new" : Expression) ~~ field.dataType. /*FIXME*/ cpp ~ "[" ~ (field.layout(0).total * field.layout(1).total * field.layout(2).total) ~ "]") : Statement))
    }

    if (Knowledge.domain_canHaveRemoteNeighs) {
      var maxPointsPerLevel = Array.fill(Knowledge.numLevels)(Array(0, 0, 0))
      var maxCommSlidesPerLevel = Array(0, 0, 0)
      for (field <- fields) {
        for (dim <- 0 until Knowledge.dimensionality) {
          maxPointsPerLevel(field.level)(dim) = math.max(maxPointsPerLevel(field.level)(dim), field.layout(dim).total - field.layout(dim).numPadLayersLeft - field.layout(dim).numPadLayersRight)
          if (Knowledge.maxLevel == field.level) {
            maxCommSlidesPerLevel(dim) = math.max(maxCommSlidesPerLevel(dim), math.max(field.layout(dim).numGhostLayersLeft, field.layout(dim).numGhostLayersRight))
            maxCommSlidesPerLevel(dim) = math.max(maxCommSlidesPerLevel(dim), math.max(field.layout(dim).numDupLayersLeft, field.layout(dim).numDupLayersRight))
          }
        }
      }

      for (neigh <- neighbors) {
        var size : String = ""
        var sizeArray = new ListBuffer[String]()
        for (i <- DimArray())
          if (0 == neigh.dir(i))
            sizeArray += s"${maxPointsPerLevel(Knowledge.maxLevel)(i)}"
          else
            sizeArray += s"${maxCommSlidesPerLevel(i)}"

        size += sizeArray.mkString(" * ")

        body += s"buffer_Send[${neigh.index}] = new double[$size]"
        body += s"buffer_Recv[${neigh.index}] = new double[$size]"
        body += s"maxElemRecvBuffer[${neigh.index}] = $size"
      }
    }

    return FunctionStatement(new UnitDatatype(), s"setupBuffers", ListBuffer(), body)
  }
}

case class SetFromExternalField(var dest : Field, var src : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetFromExternalField\n"

  override def expand : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), "set" ~ dest.codeName,
      ListBuffer(new VariableAccess("src", Some(PointerDatatype(dest.dataType))), new VariableAccess("slot", Some(new IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality).toArray.map(i => src.layout(i).idxDupLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality).toArray.map(i => src.layout(i).idxDupRightEnd))),
          new AssignmentStatement(FieldAccess(new NullExpression, dest, "slot", DefaultLoopMultiIndex()),
            ExternalFieldAccess("src", src, DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}