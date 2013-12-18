package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.knowledge._

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class WaitForMPIReq() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = WaitForMPIReq\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"waitForMPIReq",
      ListBuffer(Variable("MPI_Request*", "request")),
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

case class ExchangeDataSplitter(field : Field) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeDataSplitter\n";

  override def expand : FunctionStatement = {
    new FunctionStatement(new UnitDatatype(), s"exch${field.codeName}",
      ListBuffer(Variable("std::vector<boost::shared_ptr<Fragment3DCube> >&", "fragments"), Variable("unsigned int", "level"), Variable("unsigned int", "slot")),
      SwitchStatement("level",
        (0 to Knowledge.maxLevel).to[ListBuffer].map(level =>
          new CaseStatement(NumericLiteral(level), s"exch${field.codeName}_$level(fragments, slot);"))));
  }
}

case class ConnectLocalElement() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ConnectLocalElement\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectLocalElement",
      ListBuffer(Variable("unsigned int", "location"), Variable("boost::shared_ptr<Fragment3DCube>", "fragment")),
      ListBuffer(
        "ASSERT_WARNING((fragment), \"Invalid fragment pointer detected\", return);",
        s"neighbor_isValid[location] = true;",
        s"neighbor_isRemote[location] = false;",
        s"neighbor_localPtr[location] = fragment.get();",
        s"neighbor_fragmentId[location] = fragment->id;"))
  }
}

case class ConnectRemoteElement() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ConnectRemoteElement\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectRemoteElement",
      ListBuffer(Variable("unsigned int", "location"), Variable("exa_id_t", "id"), Variable(IntegerDatatype(), "remoteRank")),
      ListBuffer(
        s"neighbor_isValid[location] = true;",
        s"neighbor_isRemote[location] = true;",
        s"neighbor_fragmentId[location] = id;",
        s"neighbor_remoteRank[location] = remoteRank;"))
  }
}

case class SetupBuffers(var fields : ListBuffer[Field], var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n";

  override def expand : FunctionStatement = {
    var body = ListBuffer[Statement]();

    for (level <- 0 to Knowledge.maxLevel) {
      for (field <- fields) {
        for (slot <- 0 until field.numSlots) {
          body += s"${field.codeName}[$slot][$level] = new Container(Vec3u(${Mapping.numPoints(level)}, ${Mapping.numPoints(level)}, ${Mapping.numPoints(level)}), 1);"
        }
      }
    }

    for (neigh <- neighbors) {
      var size : String = "";
      var sizeArray = new ListBuffer[String]();
      for (i <- (0 to 2))
        if (0 == neigh.dir(i))
          sizeArray += s"${Mapping.numPoints(Knowledge.maxLevel)}";
        else
          sizeArray += s"${Knowledge.numGhostLayers}";

      size += sizeArray.mkString(" * ");

      body += s"buffer_Send[${neigh.index}] = new exa_real_t[$size];";
      body += s"buffer_Recv[${neigh.index}] = new exa_real_t[$size];";
      body += s"maxElemRecvBuffer[${neigh.index}] = $size;";
    }

    return FunctionStatement(new UnitDatatype(), s"setupBuffers", ListBuffer(), body);
  }
}
