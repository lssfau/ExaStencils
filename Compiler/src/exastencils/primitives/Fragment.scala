package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives._

case class IsNeighInvalid(neigh : NeighborInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"FRAG_INVALID == fragments[e]->neigh[FRAG_CUBE_${neigh.label} - FRAG_CUBE_ZN_YN_XN].location"
  }
}

class FieldForLoop(indices : IndexRange, body : Array[String]) {
  def cpp : String = {
    var s : String = "";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indices.begin(i)}; ${dimToString(i)} <= ${indices.end(i)}; ++${dimToString(i)})\n";

    s += s"{\n";
    for (stat <- body)
      s += s"$stat\n";
    s += s"}\n";

    return s;
  }
}

case class SetFieldToExpr(field : Field, level : Int, slot : Any /*FIXME: Int*/ , indices : IndexRange, expr : String) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    s += (new FieldForLoop(indices, Array(
      (new FieldAccess(field, level.toString, slot.toString, Mapping.access(indices.level))) + s" = $expr;"))).cpp;

    return s;
  }
}

class NeighborInfo(var dir : Array[Int]) {
  var label : String = (2 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");

  //  var indexInner = new IndexRange();
  //  var indexOuter = new IndexRange();
  //  var indexBorder = new IndexRange();
  //
  //  var indexOpposingInner = new IndexRange();
  //  var indexOpposingOuter = new IndexRange();
  //  var indexOpposingBorder = new IndexRange();
  //
  //  def setIndices(field : Field) {
  //    indexInner = fieldToIndexInner(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOuter = fieldToIndexOuter(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexBorder = fieldToIndexBorder(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOpposingInner = fieldToIndexInner(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOpposingOuter = fieldToIndexOuter(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  //  }
  //
  //  def setIndicesWide(field : Field) {
  //    indexInner = fieldToIndexInnerWide(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOuter = fieldToIndexOuterWide(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexBorder = fieldToIndexBorder(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOpposingInner = fieldToIndexInnerWide(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOpposingOuter = fieldToIndexOuterWide(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  //    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  //  }
  //
  //  var codeInit : String = "";
  //  var codeTreatBC : String = "";
  //  var codeExchLocal : String = "";
  //
  def addDeclarations(frag : FragmentClass) {
    for (sendOrRecv <- Array("Send", "Recv")) {
      frag.declarations += StringLiteral(s"MPI_Request request_${sendOrRecv}_$label;");
      frag.declarations += StringLiteral(s"bool reqOutstanding_${sendOrRecv}_$label;");
      frag.cTorInitList += StringLiteral(s"reqOutstanding_${sendOrRecv}_$label(false)");
    }

    frag.declarations += StringLiteral(s"exa_real_t* sendBuffer_$label;");
    frag.cTorInitList += StringLiteral(s"sendBuffer_$label(0)");
    frag.dTorBody += StringLiteral(s"if (sendBuffer_$label) { delete [] sendBuffer_$label; sendBuffer_$label = 0; }");

    frag.declarations += StringLiteral(s"exa_real_t* recvBuffer_$label;");
    frag.cTorInitList += StringLiteral(s"recvBuffer_$label(0)");
    frag.dTorBody += StringLiteral(s"if (recvBuffer_$label) { delete [] recvBuffer_$label; recvBuffer_$label = 0; }");

    frag.declarations += StringLiteral(s"int maxElemRecvBuffer_$label;");
    frag.cTorInitList += StringLiteral(s"maxElemRecvBuffer_$label(0)");
  }

  def getCode_TreatBC(field : Field, level : Int, slot : Any /*FIXME: Int*/ ) : String = {
    var code : String = "";

    if (field.bcDir0) {
      code += (new ConditionStatement(
        new IsNeighInvalid(this),
        new SetFieldToExpr(field, level, slot, fieldToIndexBorder2(dir, level), "0.0"))).cpp;
    }

    return code;
  }

  //  def addExchLocal = {
  //    codeExchLocal = "";
  //    codeExchLocal += (new ifCond(
  //      s"isValid_$label",
  //      (2 to 0 by -1).toArray.map(i =>
  //        s"for (unsigned int ${dimToString(i)} = ${indexInner.begin(i)}; ${dimToString(i)} <= ${indexInner.end(i)}; ++${dimToString(i)})\n") ++
  //        Array(
  //          s"neighMem_$label[${Mapping.access(indexOpposingOuter.level, s"(z - (${indexInner.begin(2)}) + (${indexOpposingOuter.begin(2)}))", s"(y - (${indexInner.begin(1)}) + (${indexOpposingOuter.begin(1)}))", s"(x - (${indexInner.begin(0)}) + (${indexOpposingOuter.begin(0)}))")}]" +
  //            s" = localMem[${Mapping.access(indexInner.level)}];\n"))).cpp;
  //  }
}

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
        s"FragmentNeighInfo& ref = neigh[location - FRAG_CUBE_ZN_YN_XN];",
        s"ref.location	= location;",
        s"ref.isRemote	= false;",
        s"ref.fragment	= fragment;",
        s"ref.fragId		= fragment->getId();",
        s"neighbor_isValid[location] = true;",
        s"neighbor_isRemote[location] = false;",
        s"neighbor_localPtr[location] = fragment.get();",
        s"neighbor_fragmentId[location] = fragment->getId();"))
  }
}

case class ConnectRemoteElement() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ConnectRemoteElement\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"connectRemoteElement",
      ListBuffer(Variable("FRAGMENT_LOCATION", "location"), Variable("exa_id_t", "id"), Variable(IntegerDatatype(), "remoteRank")),
      ListBuffer(
        s"FragmentNeighInfo& ref = neigh[location - FRAG_CUBE_ZN_YN_XN];",
        s"ref.location	= location;",
        s"ref.isRemote	= true;",
        s"ref.fragment	= boost::shared_ptr<CurFragmentType>();",
        s"ref.fragId		= id;",
        s"ref.remoteRank	= remoteRank;",
        s"neighbor_isValid[location] = true;",
        s"neighbor_isRemote[location] = true;",
        s"neighbor_fragmentId[location] = id;",
        s"neighbor_remoteRank[location] = remoteRank;"))
  }
}

case class SetupBuffers(fields : ListBuffer[Field]) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n";

  override def expand : FunctionStatement = {
    var body = ListBuffer[Statement]();

    for (field <- fields) {
      body += s"for (unsigned int s = 0; s < ${field.numSlots}; ++s)\n";
      body += s"${field.codeName}[s].reserve(getNumLevels());\n";
    }

    body += new ForLoopStatement(s"unsigned int l = 0", s"l <= ${Knowledge.maxLevel}", s"++l",
      ListBuffer[Statement](s"unsigned int numDataPoints = (1u << l) + 1 + 2 * NUM_GHOST_LAYERS;")
        ++ (fields.map(field =>
          new ForLoopStatement(s"unsigned int s = 0", s"s < ${field.numSlots}", "++s",
            s"${field.codeName}[s].push_back(new PayloadContainer_1Real(Vec3u(numDataPoints, numDataPoints, numDataPoints), 1));") : Statement)));

    body += s"unsigned int maxNumPointsPerDim = (1u << (NUM_LEVELS - 1)) + 1 + 2 * NUM_GHOST_LAYERS;\n";
    body += s"recvBuffer = new exa_real_t[(NUM_GHOST_LAYERS + 1) * maxNumPointsPerDim * maxNumPointsPerDim];\n";
    body += s"sendBuffer = new exa_real_t[(NUM_GHOST_LAYERS + 1) * maxNumPointsPerDim * maxNumPointsPerDim];\n";

    val neighbors : ListBuffer[(Array[Int], String)] = new ListBuffer();
    for (z <- -1 to 1) {
      for (y <- -1 to 1) {
        for (x <- -1 to 1) {
          val mod = Array("N", "0", "P")
          if (0 != x || 0 != y || 0 != z) {
            neighbors += ((Array(x, y, z), s"Z${mod(z + 1)}_Y${mod(y + 1)}_X${mod(x + 1)}"));
          }
        }
      }
    }

    for (neigh <- neighbors) {
      val neighDir = neigh._1;
      val neighName = neigh._2;

      var size : String = "";
      var sizeArray = new ListBuffer[String]();
      for (i <- (0 to 2))
        if (0 == neighDir(i))
          sizeArray += s"${Mapping.numPoints(Knowledge.maxLevel)}";
        else
          sizeArray += s"${Knowledge.numGhostLayers}";

      size += sizeArray.mkString(" * ");

      body += s"sendBuffer_$neighName = new exa_real_t[$size];\n";
      body += s"recvBuffer_$neighName = new exa_real_t[$size];\n";
      body += s"maxElemRecvBuffer_$neighName = $size;\n";
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

object fieldToIndexBorder2 extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
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
/*
object fieldToIndexInner extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS + 1"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS - NUM_GHOST_LAYERS"
      }),
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS + NUM_GHOST_LAYERS"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS - 1"
      }),
      level);
  }
}

object fieldToIndexInnerWide extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->firstDataPoint.${dimToString(i)}"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS + 1"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS - NUM_GHOST_LAYERS"
      }),
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->lastDataPoint.${dimToString(i)}"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS + NUM_GHOST_LAYERS"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS - 1"
      }),
      level);
  }
}

object fieldToIndexOuter extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)}"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS + 1"
      }),
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS - 1"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)}"
      }),
      level);
  }
}

object fieldToIndexOuterWide extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->firstDataPoint.${dimToString(i)}"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)}"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS + 1"
      }),
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->lastDataPoint.${dimToString(i)}"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS - 1"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)}"
      }),
      level);
  }
}

object fieldToIndexBorder extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS"
      }),
      range(0, 3).map(i => i match {
        case i if dir(i) == 0 => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS"
        case i if dir(i) < 0  => s"$field->firstDataPoint.${dimToString(i)} + NUM_GHOST_LAYERS"
        case i if dir(i) > 0  => s"$field->lastDataPoint.${dimToString(i)} - NUM_GHOST_LAYERS"
      }),
      level);
  }
}
*/

case class CopyLocalToBuffer(field : String, buffer : String, indices : IndexRange) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    s += "size_t entry = 0;\n";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indices.begin(i)}; ${dimToString(i)} <= ${indices.end(i)}; ++${dimToString(i)})\n";
    s += s"$buffer[entry++] = $field->data[${Mapping.access(indices.level)}];\n";
    //s += s"$buffer[entry++] = $field->data[z * $field->numDataPointsPerDimWPad.xy().componentProd() + y * $field->numDataPointsPerDimWPad.x + x];\n";

    return s;
  }
};

case class CopyBufferToLocal(field : String, buffer : String, indices : IndexRange) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    s += "size_t entry = 0;\n";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indices.begin(i)}; ${dimToString(i)} <= ${indices.end(i)}; ++${dimToString(i)})\n";
    s += s"$field->data[${Mapping.access(indices.level)}] = $buffer[entry++];\n";
    //s += s"$field->data[z * $field->numDataPointsPerDimWPad.xy().componentProd() + y * $field->numDataPointsPerDimWPad.x + x] = $buffer[entry++];\n";

    return s;
  }
};

case class CopyLocalToLocal(fieldSrc : String, indicesSrc : IndexRange, fieldDest : String, indicesDest : IndexRange) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indicesSrc.begin(i)}; ${dimToString(i)} <= ${indicesSrc.end(i)}; ++${dimToString(i)})\n";
    s += s"$fieldDest->data[${Mapping.access(indicesDest.level, s"(z - (${indicesSrc.begin(2)}) + (${indicesDest.begin(2)}))", s"(y - (${indicesSrc.begin(1)}) + (${indicesDest.begin(1)}))", s"(x - (${indicesSrc.begin(0)}) + (${indicesDest.begin(0)}))")}]" +
      s" = $fieldSrc->data[${Mapping.access(indicesSrc.level)}];\n";
    //s += s"$fieldDest->data[(z - (${indicesSrc.begin(2)}) + (${indicesDest.begin(2)})) * $fieldDest->numDataPointsPerDimWPad.xy().componentProd() + (y - (${indicesSrc.begin(1)}) + (${indicesDest.begin(1)})) * $fieldDest->numDataPointsPerDimWPad.x + x - (${indicesSrc.begin(0)}) + (${indicesDest.begin(0)})]" +
    //  s" = $fieldSrc->data[z * $fieldSrc->numDataPointsPerDimWPad.xy().componentProd() + y * $fieldSrc->numDataPointsPerDimWPad.x + x];\n";

    return s;
  }
};

//class CopyLocalToLocalHACK(fieldSrc : String, indicesSrc : IndexRange, fieldDest : String, indicesDest : IndexRange) {
//  def cpp : String = {
//    var s : String = "";
//
//    for (i <- 2 to 0 by -1)
//      s += s"for (unsigned int ${dimToString(i)} = ${indicesSrc.begin(i)}; ${dimToString(i)} <= ${indicesSrc.end(i)}; ++${dimToString(i)})\n";
//    s += s"$fieldDest[${Mapping.access(indicesDest.level, s"(z - (${indicesSrc.begin(2)}) + (${indicesDest.begin(2)}))", s"(y - (${indicesSrc.begin(1)}) + (${indicesDest.begin(1)}))", s"(x - (${indicesSrc.begin(0)}) + (${indicesDest.begin(0)}))")}]" +
//      s" = $fieldSrc[${Mapping.access(indicesSrc.level)}];\n";
//
//    return s;
//  }
//};

case class CopyLocalToLocalHACK(fieldSrc : String, indicesSrc : IndexRange, fieldDest : String, indicesDest : IndexRange) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indicesSrc.begin(i)}; ${dimToString(i)} <= ${indicesSrc.end(i)}; ++${dimToString(i)})\n";
    s += s"$fieldDest[${Mapping.access(indicesDest.level, s"(z - (${indicesSrc.begin(2)}) + (${indicesDest.begin(2)}))", s"(y - (${indicesSrc.begin(1)}) + (${indicesDest.begin(1)}))", s"(x - (${indicesSrc.begin(0)}) + (${indicesDest.begin(0)}))")}]" +
      s" = $fieldSrc[${Mapping.access(indicesSrc.level)}];\n";

    return s;
  }
};

case class SetDataZero(field : String, indices : IndexRange) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indices.begin(i)}; ${dimToString(i)} <= ${indices.end(i)}; ++${dimToString(i)})\n";
    s += s"$field->data[${Mapping.access(indices.level)}] = 0.0;\n";
    //s += s"$field->data[z * $field->numDataPointsPerDimWPad.xy().componentProd() + y * $field->numDataPointsPerDimWPad.x + x] = 0.0;\n";

    return s;
  }
};

case class SendBuffer(buffer : String, rank : String, tag : String, synchronous : Boolean = true, request : String = "request") extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    // TODO: calculate number of entries

    s += s"#pragma omp critical\n{\n";

    if (synchronous) {
      s += s"MPI_Isend($buffer, entry, MPI_DOUBLE, $rank, $tag, MPI_COMM_WORLD, &$request);\n";
      //s += "reqOutstanding = true;\n";
    } else {
      s += "NOT IMPLEMENTED YET\n";
    }

    s += s"}\n";

    return s;
  }
};

case class RecvBuffer(buffer : String, rank : String, tag : String, size : String, asynchronous : Boolean = true, request : String = "request") extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    // TODO: calculate number of entries

    s += s"#pragma omp critical\n{\n";

    if (asynchronous) {
      s += s"MPI_Irecv($buffer, $size, MPI_DOUBLE, $rank, $tag, MPI_COMM_WORLD, &$request);\n";
    } else {
      s += "MPI_Status stat;\n";
      s += s"if (MPI_ERR_IN_STATUS == MPI_Recv($buffer, $size, MPI_DOUBLE, $rank, $tag, MPI_COMM_WORLD, &stat))\n{\n";
      s += "char msg[MPI_MAX_ERROR_STRING];"
      s += "int len;"
      s += "MPI_Error_string(stat.MPI_ERROR, msg, &len);"
      s += "LOG_WARNING(\"MPI Error encountered (\" << msg << \")\");\n";
      s += "}\n"
    }

    s += s"}\n";

    return s;
  }
};

case class TreatNeighSend(field : Field, neighName : String, indicesLocal : IndexRange, indicesNeigh : IndexRange, level : Int) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    s += (new ConditionStatement(
      s"FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location",
      ListBuffer[Statement](
        StringLiteral(s"FragmentNeighInfo& curNeigh = fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN];"),
        new ConditionStatement(s"curNeigh.isRemote",
          ListBuffer[Statement](
            //"if (1 == mpiRank) LOG_NOTE(\"Sending from \" << fragments[e]->id << \" to \" << curNeigh.fragId);",
            new CopyLocalToBuffer(s"fragments[e]->${field.codeName}[slot][$level]", s"fragments[e]->sendBuffer_$neighName", indicesLocal),
            new SendBuffer(s"fragments[e]->sendBuffer_$neighName", s"curNeigh.remoteRank", s"((unsigned int)fragments[e]->id << 16) + ((unsigned int)curNeigh.fragId & 0x0000ffff)", true, s"fragments[e]->request_Send_$neighName"),
            StringLiteral(s"fragments[e]->reqOutstanding_Send_$neighName = true;")),
          new CopyLocalToLocal(s"fragments[e]->${field.codeName}[slot][$level]", indicesLocal, s"curNeigh.fragment->get${field.codeName}($level, slot)", indicesNeigh))))).cpp;
    return s;
  }
}

case class TreatNeighSendRemote(field : Field, neighName : String, indicesLocal : IndexRange, indicesNeigh : IndexRange, level : Int) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    s += (new ConditionStatement(
      s"FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location && fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].isRemote",
      ListBuffer[Statement](
        StringLiteral(s"FragmentNeighInfo& curNeigh = fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN];"),
        //"if (1 == mpiRank) LOG_NOTE(\"Sending from \" << fragments[e]->id << \" to \" << curNeigh.fragId);",
        new CopyLocalToBuffer(s"fragments[e]->${field.codeName}[slot][$level]", s"fragments[e]->sendBuffer_$neighName", indicesLocal),
        new SendBuffer(s"fragments[e]->sendBuffer_$neighName", s"curNeigh.remoteRank", s"((unsigned int)fragments[e]->id << 16) + ((unsigned int)curNeigh.fragId & 0x0000ffff)", true, s"fragments[e]->request_Send_$neighName"),
        StringLiteral(s"fragments[e]->reqOutstanding_Send_$neighName = true;")))).cpp;
    return s;
  }
}

case class TreatNeighSendLocal(field : Field, neighName : String, indicesLocal : IndexRange, indicesNeigh : IndexRange, level : Int) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    s += (new ConditionStatement(
      s"isValid_$neighName",
      new CopyLocalToLocalHACK(s"localMem", indicesLocal, s"neighMem_$neighName", indicesNeigh))).cpp;
    return s;
  }
}

case class TreatNeighRecv(field : Field, neighName : String, indices : IndexRange, level : Int) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";
    s += (new ConditionStatement(
      s"FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location",
      ListBuffer[Statement](
        StringLiteral(s"FragmentNeighInfo& curNeigh = fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN];"),
        new ConditionStatement(s"curNeigh.isRemote",
          ListBuffer[Statement](
            //"if (1 == mpiRank) LOG_NOTE(\"Receiving from \" << curNeigh.fragId << \" to \" << fragments[e]->id);",
            new RecvBuffer(s"fragments[e]->recvBuffer_$neighName", s"curNeigh.remoteRank", s"((unsigned int)curNeigh.fragId << 16) + ((unsigned int)fragments[e]->id & 0x0000ffff)",
              s"NUM_GHOST_LAYERS * fragments[e]->${field.codeName}[slot][$level]->numDataPointsPerDim.y * fragments[e]->${field.codeName}[slot][$level]->numDataPointsPerDim.z", false),
            new CopyBufferToLocal(s"fragments[e]->${field.codeName}[slot][$level]", s"fragments[e]->recvBuffer_$neighName", indices)))))).cpp;
    return s;
  }
}

case class TreatNeighFinish(neighName : String) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    for (sendOrRecv <- Array("Send", "Recv")) {
      s += (new ConditionStatement(s"fragments[e]->reqOutstanding_${sendOrRecv}_$neighName",
        ListBuffer[Statement](
          StringLiteral(s"#pragma omp critical"),
          StringLiteral(s"{"),
          StringLiteral(s"waitForMPIReq(&fragments[e]->request_${sendOrRecv}_$neighName);"),
          StringLiteral(s"}"),
          StringLiteral(s"fragments[e]->reqOutstanding_${sendOrRecv}_$neighName = false;")))).cpp;
    }

    return s;
  }
}

case class TreatNeighBC(field : Field, neighName : String, indices : IndexRange, level : Int) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    if (field.bcDir0) {
      s += (new ConditionStatement(
        s"FRAG_INVALID == fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location",
        new SetDataZero(s"fragments[e]->${field.codeName}[slot][$level]", indices))).cpp;
    }
    return s;
  }
}

class NeighInfo(var dir : Array[Int], var level : Int, var index : Int) {
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

  var codeInit : Statement = "";
  var codeTreatBC : Statement = "";
  var codeExchLocal : Statement = "";

  //  def addCodeInit(field : Field) {
  //    codeInit += s"isValid_$label = (FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].location && !fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].isRemote);\n";
  //    codeInit += s"neighMem_$label = isValid_$label ? fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].fragment->get${field.codeName}($level, slot)->data : 0;\n";
  //  }

  def addTreatBC(field : Field) {
    if (field.bcDir0) {
      codeTreatBC = new ConditionStatement(
        s"FRAG_INVALID == fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].location",
        new SetDataZero(s"fragments[e]->${field.codeName}[slot][$level]", indexBorder));
    }
  }

  def addExchLocal = {
    var stats : ListBuffer[Statement] = ListBuffer();
    (2 to 0 by -1).toArray.map(i =>
      stats += StringLiteral(s"for (unsigned int ${dimToString(i)} = ${indexInner.begin(i)}; ${dimToString(i)} <= ${indexInner.end(i)}; ++${dimToString(i)})"));
    stats += StringLiteral(s"neighMem_$label[${Mapping.access(indexOpposingOuter.level, s"(z - (${indexInner.begin(2)}) + (${indexOpposingOuter.begin(2)}))", s"(y - (${indexInner.begin(1)}) + (${indexOpposingOuter.begin(1)}))", s"(x - (${indexInner.begin(0)}) + (${indexOpposingOuter.begin(0)}))")}]" +
      s" = localMem[${Mapping.access(indexInner.level)}];");

    codeExchLocal = new ConditionStatement(s"isValid_$label", stats);
  }

  // (new TreatNeighRecv(field, neigh.label, neigh.indexOuter, level)).cpp).toArray)).cpp;

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

case class ExchangeData_6(field : Field, level : Int) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_6\n";

  override def expand : FunctionStatement = {
    var body = new ListBuffer[Statement];

    // FIXME clean-up
    val fieldName = s"fragments[e]->${field.codeName}[slot][$level]";

    // simple exchange along axis
    val neighbors = Array( // FIXME: indices
      new NeighInfo(Array(-1, 0, 0), level, 4), new NeighInfo(Array(1, 0, 0), level, 22),
      new NeighInfo(Array(0, -1, 0), level, 10), new NeighInfo(Array(0, 1, 0), level, 16),
      new NeighInfo(Array(0, 0, -1), level, 12), new NeighInfo(Array(0, 0, 1), level, 14));

    for (neigh <- neighbors) {
      neigh.setIndicesWide(field);
    }

    // handle BC
    body += (new LoopOverFragments(
      neighbors.map(neigh => (
        new TreatNeighBC(field, neigh.label,
          fieldToIndexBorder(neigh.dir, fieldName, level), level)) : Statement).to[ListBuffer]));

    // sync duplicate values
    for (dim <- 0 to 2) {
      body += (new LoopOverFragments(
        ListBuffer[Statement](
          (new TreatNeighSend(field, neighbors(2 * dim + 1).label,
            neighbors(2 * dim + 1).indexBorder,
            neighbors(2 * dim + 1).indexOpposingBorder, level)))));

      body += new LoopOverFragments(new TreatNeighRecv(field, neighbors(2 * dim + 0).label, neighbors(2 * dim + 0).indexBorder, level));
      body += new LoopOverFragments(new TreatNeighFinish(neighbors(2 * dim + 1).label));
    }

    // update ghost layers
    for (dim <- 0 to 2) {
      body += new LoopOverFragments(
        Array(0, 1).map(dir => (new TreatNeighSend(field, neighbors(2 * dim + dir).label, neighbors(2 * dim + dir).indexInner,
          neighbors(2 * dim + dir).indexOpposingOuter, level) : Statement)).to[ListBuffer]);

      body += new LoopOverFragments(
        Array(0, 1).map(dir => (new TreatNeighRecv(field, neighbors(2 * dim + dir).label, neighbors(2 * dim + dir).indexOuter, level) : Statement)).to[ListBuffer]);

      body += new LoopOverFragments(
        Array(0, 1).map(dir => (new TreatNeighFinish(neighbors(2 * dim + dir).label) : Statement)).to[ListBuffer]);
    }

    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_$level",
      ListBuffer(Variable("std::vector<boost::shared_ptr<CurFragmentType> >&", "fragments"), Variable("unsigned int", "slot")),
      body);
  }
}

case class HandleBoundaries(neighbors : ListBuffer[NeighInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = { return "NOT VALID ; CLASS = HandleBoundaries\n"; }

  override def expand : StatementBlock = { return new StatementBlock(neighbors.map(neigh => neigh.codeTreatBC)); }
}

case class ExchangeData_26(field : Field, level : Int) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_26\n";

  override def expand : FunctionStatement = {
    var body = new ListBuffer[Statement];

    // FIXME clean-up

    val fieldName = s"fragments[e]->${field.codeName}[slot][$level]";

    // TODO: get neighbors from parent
    val neighbors = new ListBuffer[NeighInfo](); //FragmentClass.neighbors;
    for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
      neighbors += new NeighInfo(Array(x, y, z), level, (z + 1) * 9 + (y + 1) * 3 + (x + 1));
    }

    for (neigh <- neighbors) {
      neigh.setIndices(field);
      neigh.addTreatBC(field);
      neigh.addExchLocal;

    }

    //body += "int mpiRank; MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);\n";

    // handle BC
    body += new LoopOverFragments(new HandleBoundaries(neighbors));

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

    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_$level",
      ListBuffer(Variable("std::vector<boost::shared_ptr<CurFragmentType> >&", "fragments"), Variable("unsigned int", "slot")),
      body);
  }
}
