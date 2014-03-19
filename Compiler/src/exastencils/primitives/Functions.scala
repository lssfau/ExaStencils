package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class WaitForMPIReq() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIReq\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"waitForMPIReq",
      ListBuffer(VariableAccess("request", Some("MPI_Request*"))),
      ListBuffer(
        s"MPI_Status stat;",
        s"if (MPI_ERR_IN_STATUS == MPI_Wait(request, &stat))\n{",
        s"char msg[MPI_MAX_ERROR_STRING];",
        s"int len;",
        s"MPI_Error_string(stat.MPI_ERROR, msg, &len);",
        "LOG_WARNING(\"MPI Error encountered (\" << msg << \")\");",
        s"}",
        s"*request = MPI_Request();"))
  }
}

case class WaitForMPISendOps(var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPISendOps\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    if (Knowledge.comm_useLoopsOverNeighbors) {
      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index;
      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index;

      new FunctionStatement(new UnitDatatype(), s"waitForMPISendOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(
          new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
            new ConditionStatement(s"curFragment.reqOutstanding_Send[i]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Send[i]);",
                s"curFragment.reqOutstanding_Send[i] = false;")))));
    } else {
      new FunctionStatement(new UnitDatatype(), s"waitForMPISendOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(
          neighbors.map(neigh =>
            new ConditionStatement(s"curFragment.reqOutstanding_Send[${neigh.index}]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Send[${neigh.index}]);",
                s"curFragment.reqOutstanding_Send[${neigh.index}] = false;")) : Statement)));
    }
  }
}

case class WaitForMPIRecvOps(var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIRecvOps\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    if (Knowledge.comm_useLoopsOverNeighbors) {
      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index;
      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index;

      new FunctionStatement(new UnitDatatype(), s"waitForMPIRecvOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(
          new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i",
            new ConditionStatement(s"curFragment.reqOutstanding_Recv[i]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Recv[i]);",
                s"curFragment.reqOutstanding_Recv[i] = false;")))));
    } else {
      new FunctionStatement(new UnitDatatype(), s"waitForMPIRecvOps", ListBuffer[VariableAccess](),
        new LoopOverFragments(
          neighbors.map(neigh =>
            new ConditionStatement(s"curFragment.reqOutstanding_Recv[${neigh.index}]",
              ListBuffer[Statement](
                s"waitForMPIReq(&curFragment.request_Recv[${neigh.index}]);",
                s"curFragment.reqOutstanding_Recv[${neigh.index}] = false;")) : Statement)));
    }
  }
}

case class ExchangeDataSplitter(field : Field) extends AbstractFunctionStatement with Expandable {
  // TODO: this function will become obsolete when the MG components are fully implemented
  override def cpp : String = "NOT VALID ; CLASS = ExchangeDataSplitter\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), s"exch${field.codeName.cpp}",
      ListBuffer(VariableAccess("level", Some("unsigned int")), VariableAccess("slot", Some("unsigned int"))),
      SwitchStatement("level",
        (0 to Knowledge.maxLevel).to[ListBuffer].map(level =>
          new CaseStatement(level, s"exch${field.codeName.cpp}_$level(slot);"))));
  }
}

case class ConnectLocalElement() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectLocalElement\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectLocalElement",
      ListBuffer(VariableAccess("location", Some("unsigned int")), VariableAccess("fragment", Some("Fragment3DCube*"))),
      ListBuffer(
        "ASSERT_WARNING((fragment), \"Invalid fragment pointer detected\", return);",
        s"neighbor_isValid[location] = true;",
        s"neighbor_isRemote[location] = false;",
        s"neighbor_localPtr[location] = fragment;",
        s"neighbor_fragmentId[location] = fragment->id;"))
  }
}

case class ConnectRemoteElement() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectRemoteElement\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectRemoteElement",
      ListBuffer(VariableAccess("location", Some("unsigned int")), VariableAccess("id", Some("size_t")), VariableAccess("remoteRank", Some(IntegerDatatype()))),
      ListBuffer(
        s"neighbor_isValid[location] = true;",
        s"neighbor_isRemote[location] = true;",
        s"neighbor_fragmentId[location] = id;",
        s"neighbor_remoteRank[location] = remoteRank;"))
  }
}

case class SetupBuffers(var fields : ListBuffer[Field], var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    var body = ListBuffer[Statement]();

    for (field <- fields) {
      for (slot <- 0 until field.numSlots) {
        body += s"${field.codeName.cpp}[$slot][${field.level}] = new Container(Vec3u(${field.layout(0).total}, ${field.layout(1).total}, ${field.layout(2).total}), 1);"
      }
    }

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
      var size : String = "";
      var sizeArray = new ListBuffer[String]();
      for (i <- (0 until Knowledge.dimensionality))
        if (0 == neigh.dir(i))
          sizeArray += s"${maxPointsPerLevel(Knowledge.maxLevel)(i)}";
        else
          sizeArray += s"${maxCommSlidesPerLevel(i)}";

      size += sizeArray.mkString(" * ");

      body += s"buffer_Send[${neigh.index}] = new double[$size];";
      body += s"buffer_Recv[${neigh.index}] = new double[$size];";
      body += s"maxElemRecvBuffer[${neigh.index}] = $size;";
    }

    return FunctionStatement(new UnitDatatype(), s"setupBuffers", ListBuffer(), body);
  }
}
