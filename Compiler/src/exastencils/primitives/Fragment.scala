package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives._

object dimToString extends (Int => String) {
  def apply(dim : Int) : String = {
    return dim match {
      case 0 => "x";
      case 1 => "y";
      case 2 => "z";
      case _ => "UNKNOWN";
    }
  }
};

object dirToString extends (Int => String) {
  def apply(dim : Int) : String = {
    return dim match {
      case -1 => "N";
      case 0  => "0";
      case 1  => "P";
      case _  => "UNKNOWN";
    }
  }
};

case class ConnectLocalElement() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ConnectLocalElement\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectLocalElement", // FIXME: set prefix as class trafo 
      ListBuffer(Variable("FRAGMENT_LOCATION", "location"), Variable("boost::shared_ptr<CurFragmentType>", "fragment")),
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
      ListBuffer(Variable("FRAGMENT_LOCATION", "location"), Variable("exa_id_t", "id"), Variable(IntegerDatatype(), "remoteRank")),
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

    for (field <- fields) {
      body += s"for (unsigned int s = 0; s < ${field.numSlots}; ++s)\n";
      body += s"${field.codeName}[s].reserve(${Knowledge.maxLevel + 1});\n";
    }

    body += new ForLoopStatement(s"unsigned int l = 0", s"l <= ${Knowledge.maxLevel}", s"++l",
      ListBuffer[Statement](s"unsigned int numDataPoints = (1u << l) + 1 + 2 * NUM_GHOST_LAYERS;")
        ++ (fields.map(field =>
          new ForLoopStatement(s"unsigned int s = 0", s"s < ${field.numSlots}", "++s",
            s"${field.codeName}[s].push_back(new PayloadContainer_1Real(Vec3u(numDataPoints, numDataPoints, numDataPoints), 1));") : Statement)));

    for (neigh <- neighbors) {
      var size : String = "";
      var sizeArray = new ListBuffer[String]();
      for (i <- (0 to 2))
        if (0 == neigh.dir(i))
          sizeArray += s"${Mapping.numPoints(Knowledge.maxLevel)}";
        else
          sizeArray += s"${Knowledge.numGhostLayers}";

      size += sizeArray.mkString(" * ");

      body += s"buffer_Send[${neigh.index}] = new exa_real_t[$size];\n";
      body += s"buffer_Recv[${neigh.index}] = new exa_real_t[$size];\n";
      body += s"maxElemRecvBuffer[${neigh.index}] = $size;\n";
    }

    return FunctionStatement(new UnitDatatype(), s"setupBuffers", ListBuffer(), body);
  }
}

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

case class IndexRange(begin : Array[String] = Array("0", "0", "0"), end : Array[String] = Array("0", "0", "0"), level : Int = 0) {}

object Mapping {
  def first(level : Int) : Int = {
    return 0;
  }
  def last(level : Int) : Int = {
    return numPoints(level) - 1;
  }
  def numPoints(level : Int) : Int = {
    return (1 << level) + 1 + 2 * Knowledge.numGhostLayers;
  }
  def access(level : Int, z : String = "z", y : String = "y", x : String = "x") : String = {
    return s"$z * (${numPoints(level)} * ${numPoints(level)}) + $y * ${numPoints(level)} + $x";
  }
}

object fieldToIndexInner extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - Knowledge.numGhostLayers}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - 1}"
      }),
      level);
  }
}

object fieldToIndexInnerWide extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - Knowledge.numGhostLayers}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - 1}"
      }),
      level);
  }
}

object fieldToIndexOuter extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level)}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers + 1}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers - 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level)}"
      }),
      level);
  }
}

object fieldToIndexOuterWide extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level)}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers + 1}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers - 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level)}"
      }),
      level);
  }
}

object fieldToIndexBorder extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
      }),
      level);
  }
}

class NeighborInfo(var dir : Array[Int], var level : Int /*FIXME: remove level*/ , var index : Int) {
  var label : String = (2 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");

  var indexInner = new IndexRange();
  var indexOuter = new IndexRange();
  var indexBorder = new IndexRange();

  var indexOpposingInner = new IndexRange();
  var indexOpposingOuter = new IndexRange();
  var indexOpposingBorder = new IndexRange();

  def setIndices(field : Field) {
    indexInner = fieldToIndexInner(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOuter = fieldToIndexOuter(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexBorder = fieldToIndexBorder(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingInner = fieldToIndexInner(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingOuter = fieldToIndexOuter(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  }

  def setIndicesWide(field : Field) {
    indexInner = fieldToIndexInnerWide(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOuter = fieldToIndexOuterWide(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexBorder = fieldToIndexBorder(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingInner = fieldToIndexInnerWide(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingOuter = fieldToIndexOuterWide(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  }
}

case class ExchangeDataSplitter(field : Field) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeDataSplitter\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"exch${field.codeName}",
      ListBuffer(Variable("std::vector<boost::shared_ptr<CurFragmentType> >&", "fragments"), Variable("unsigned int", "level"), Variable("unsigned int", "slot")),
      // FIXME: this needs to be facilitated; TODO: add SwitchStatement node
      ListBuffer(ExpressionStatement(StringLiteral(s"switch (level)\n{"))) ++
        ((0 to Knowledge.maxLevel).toList.map(level =>
          ExpressionStatement(StringLiteral(s"case $level: exch${field.codeName}_$level(fragments, slot);\nbreak;"))).toList) ++
        ListBuffer(ExpressionStatement(StringLiteral(s"}"))))
  }
}

case class ExchangeData_6(field : Field, level : Int, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_6\n";

  override def expand : FunctionStatement = {
    var body = new ListBuffer[Statement];

    val fieldName = s"fragments[e]->${field.codeName}[slot][$level]";

    // simple exchange along axis
    //    val neighbors = new ListBuffer[NeighborInfo]();
    //    neighbors += new NeighborInfo(Array(-1, 0, 0), level, 12);
    //    neighbors += new NeighborInfo(Array(+1, 0, 0), level, 14);
    //    neighbors += new NeighborInfo(Array(0, -1, 0), level, 10);
    //    neighbors += new NeighborInfo(Array(0, +1, 0), level, 16);
    //    neighbors += new NeighborInfo(Array(0, 0, -1), level, 4);
    //    neighbors += new NeighborInfo(Array(0, 0, +1), level, 22);

    for (neigh <- neighbors) {
      neigh.level = level; // FIXME: remove level
      neigh.setIndicesWide(field);
    }

    // handle BC
    body += new HandleBoundaries(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.map(neigh => (neigh, neigh.indexBorder)));

    // sync duplicate values
    for (dim <- 0 to 2) {
      body += new CopyToSendBuffer_and_RemoteSend(field, ImplicitConversions.NumberToNumericLiteral(level),
        ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, neigh.indexBorder)));
      body += new LocalSend(field, ImplicitConversions.NumberToNumericLiteral(level),
        ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, neigh.indexBorder, neigh.indexOpposingBorder)));

      body += new RemoteReceive(field, level, ListBuffer(neighbors(2 * dim + 0)));

      body += new FinishRemoteCommunication(neighbors);

      body += new CopyFromRecvBuffer(field, ImplicitConversions.NumberToNumericLiteral(level),
        ListBuffer(neighbors(2 * dim + 0)).map(neigh => (neigh, neigh.indexBorder)));
    }

    // update ghost layers
    for (dim <- 0 to 2) {
      var curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1));

      body += new CopyToSendBuffer_and_RemoteSend(field, ImplicitConversions.NumberToNumericLiteral(level),
        curNeighbors.map(neigh => (neigh, neigh.indexInner)));

      body += new LocalSend(field, ImplicitConversions.NumberToNumericLiteral(level),
        curNeighbors.map(neigh => (neigh, neigh.indexInner, neigh.indexOpposingOuter)));

      body += new RemoteReceive(field, level, curNeighbors);

      body += new FinishRemoteCommunication(curNeighbors);

      body += new CopyFromRecvBuffer(field, ImplicitConversions.NumberToNumericLiteral(level),
        curNeighbors.map(neigh => (neigh, neigh.indexOuter)));
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_$level",
      ListBuffer(Variable("std::vector<boost::shared_ptr<CurFragmentType> >&", "fragments"), Variable("unsigned int", "slot")),
      body);
  }
}

case class ExchangeData_26(field : Field, level : Int, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_26\n";

  override def expand : FunctionStatement = {
    var body = new ListBuffer[Statement];

    val fieldName = s"fragments[e]->${field.codeName}[slot][$level]";

    for (neigh <- neighbors) {
      neigh.level = level; // FIXME: remove level
      neigh.setIndices(field);
    }

    // handle BC
    body += new HandleBoundaries(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.map(neigh => (neigh, neigh.indexBorder)));

    // sync duplicate values
    body += new CopyToSendBuffer_and_RemoteSend(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, neigh.indexBorder)));
    body += new LocalSend(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, neigh.indexBorder, neigh.indexOpposingBorder)));

    body += new RemoteReceive(field, level, neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0));

    body += new FinishRemoteCommunication(neighbors);

    body += new CopyFromRecvBuffer(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0).map(neigh => (neigh, neigh.indexBorder)));

    // update ghost layers
    body += new CopyToSendBuffer_and_RemoteSend(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.map(neigh => (neigh, neigh.indexInner)));
    body += new LocalSend(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.map(neigh => (neigh, neigh.indexInner, neigh.indexOpposingOuter)));

    body += new RemoteReceive(field, level, neighbors);

    body += new FinishRemoteCommunication(neighbors);

    body += new CopyFromRecvBuffer(field, ImplicitConversions.NumberToNumericLiteral(level),
      neighbors.map(neigh => (neigh, neigh.indexOuter)));

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_$level",
      ListBuffer(Variable("std::vector<boost::shared_ptr<CurFragmentType> >&", "fragments"), Variable("unsigned int", "slot")),
      body);
  }
}
