package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.primitives._

object Knowledge {
  val numGhostLayers = 1;
}

class IsNeighValid(neigh : NeighborInfo) {
  def toString_cpp : String = {
    var s : String = "";

    //s += s"fragments[e]->isValidNeigh_${neigh.label}";
    s += s"FRAG_INVALID == fragments[e]->neigh[FRAG_CUBE_${neigh.label} - FRAG_CUBE_ZN_YN_XN].location";

    return s;
  }
}

class FieldForLoop(indices : IndexRange, body : Array[String]) {
  def toString_cpp : String = {
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

class FieldAccess(field : Field, level : Int, slot : Any/*FIXME: Int*/, index : String) {
  def toString_cpp : String = {
    var s : String = "";

    s += s"fragments[e]->${field.codeName}[$slot][$level]->data[$index]";

    return s;
  }
}

class SetFieldToExpr(field : Field, level : Int, slot : Any/*FIXME: Int*/, indices : IndexRange, expr : String) {
  def toString_cpp : String = {
    var s : String = "";

    s += (new FieldForLoop(indices, Array(
      (new FieldAccess(field, level, slot, Mapping.access(indices.level))).toString_cpp + s" = $expr;"))).toString_cpp;

    return s;
  }
}

class NeighborInfo(var dir : Array[Int]) {
  var label : String = (2 to 0 by -1).toArray.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");

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
  def addDeclarations() {
    FragmentClass.declarations += s"MPI_Request request_$label;";
    FragmentClass.declarations += s"bool reqOutstanding_$label;";
    FragmentClass.cTorInitList += s"reqOutstanding_$label(false)";

    FragmentClass.declarations += s"exa_real_t* sendBuffer_$label;";
    FragmentClass.cTorInitList += s"sendBuffer_$label(0)";
    FragmentClass.dTorBody += s"if (sendBuffer_$label) { delete [] sendBuffer_$label; sendBuffer_$label = 0; }";

    FragmentClass.declarations += s"exa_real_t* recvBuffer_$label;";
    FragmentClass.cTorInitList += s"recvBuffer_$label(0)";
    FragmentClass.dTorBody += s"if (recvBuffer_$label) { delete [] recvBuffer_$label; recvBuffer_$label = 0; }";
  }

  def getCode_TreatBC(field : Field, level : Int, slot : Any/*FIXME: Int*/) : String = {
    var code : String = "";

    if (field.bcDir0) {
      code += (new ifCond(
        (new IsNeighValid(this)).toString_cpp,
        Array((new SetFieldToExpr(field, level, slot, fieldToIndexBorder2(dir, level), "0.0")).toString_cpp))).toString_cpp;
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
  //            s" = localMem[${Mapping.access(indexInner.level)}];\n"))).toString_cpp;
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

case class ifCond(cond : String, trueBranch : Array[String], falseBranch : Array[String] = Array[String]()) {
  def toString_cpp : String = {
    var s : String = "";

    s += s"if ($cond)\n{\n";
    for (stat <- trueBranch)
      s += s"$stat\n";
    s += s"}\n";
    if (falseBranch.length > 0) {
      s += s"else\n{\n";
      for (stat <- falseBranch)
        s += s"$stat\n";
      s += s"}\n";
    }

    return s;
  }
}

case class forLoop(head : String, body : Array[String]) {
  def toString_cpp : String = {
    var s : String = "";

    // HACK
    if ("int e = 0; e < fragments.size(); ++e" == head) {
      s += "#pragma omp parallel for schedule(static, 1)\n";
    }

    s += s"for ($head)\n{\n";
    for (stat <- body)
      s += s"$stat\n";
    s += s"}\n";

    return s;
  }
}

case class Field(name : String, codeName : String, dataType : String, numSlots : String, bcDir0 : Boolean) {}

class ConnectLocalElement() {
  def toString_cpp : String = {
    var s : String = "";

    s += s"void Fragment3DCube::connectLocalElement (FRAGMENT_LOCATION location, boost::shared_ptr<CurFragmentType> fragment)\n\t{\n";
    s += "ASSERT_WARNING((fragment), \"Invalid fragment pointer detected\", return);\n";

    s += s"FragmentNeighInfo& ref = neigh[location - FRAG_CUBE_ZN_YN_XN];\n";
    s += s"ref.location	= location;\n";
    s += s"ref.isRemote	= false;\n";
    s += s"ref.fragment	= fragment;\n";
    s += s"ref.fragId		= fragment->getId();\n";

    s += s"\t}\n";

    return s;
  }
}

class ConnectRemoteElement() {
  def toString_cpp : String = {
    var s : String = "";

    s += s"void Fragment3DCube::connectRemoteElement (FRAGMENT_LOCATION location, exa_id_t id, int remoteRank)\n\t{\n";

    s += s"FragmentNeighInfo& ref = neigh[location - FRAG_CUBE_ZN_YN_XN];\n";
    s += s"ref.location	= location;\n";
    s += s"ref.isRemote	= true;\n";
    s += s"ref.fragment	= boost::shared_ptr<CurFragmentType>();\n";
    s += s"ref.fragId		= id;\n";
    s += s"ref.remoteRank	= remoteRank;\n";

    s += s"\t}\n";

    return s;
  }
}

class SetupBuffers(fields : ListBuffer[Field]) {
  def toString_cpp : String = {
    var s : String = "";

    s += s"void Fragment3DCube::setupBuffers ()\n\t{\n";

    for (field <- fields) {
      s += s"for (unsigned int s = 0; s < ${field.numSlots}; ++s)\n";
      s += s"${field.codeName}[s].reserve(getNumLevels());\n";
    }

    s += s"for (unsigned int l = 0; l < NUM_LEVELS; ++l)\n{\n";
    s += s"unsigned int numDataPoints = (1u << l) + 1 + 2 * NUM_GHOST_LAYERS;\n";
    for (field <- fields) {
      s += s"for (unsigned int s = 0; s < ${field.numSlots}; ++s)\n";
      s += s"${field.codeName}[s].push_back(new PayloadContainer_1Real(Vec3u(numDataPoints, numDataPoints, numDataPoints), 1));\n";
      //s += s"${field.codeName}[s].push_back(boost::shared_ptr<PayloadContainer_1Real>(new PayloadContainer_1Real(Vec3u(numDataPoints, numDataPoints, numDataPoints), 1)));\n";
    }
    s += s"}\n";

    s += s"unsigned int maxNumPointsPerDim = (1u << (NUM_LEVELS - 1)) + 1 + 2 * NUM_GHOST_LAYERS;\n";
    s += s"recvBuffer = new exa_real_t[(NUM_GHOST_LAYERS + 1) * maxNumPointsPerDim * maxNumPointsPerDim];\n";
    s += s"sendBuffer = new exa_real_t[(NUM_GHOST_LAYERS + 1) * maxNumPointsPerDim * maxNumPointsPerDim];\n";
    //s += s"recvBuffer = boost::shared_array<exa_real_t>(new exa_real_t[(NUM_GHOST_LAYERS + 1) * maxNumPointsPerDim * maxNumPointsPerDim]);\n";
    //s += s"sendBuffer = boost::shared_array<exa_real_t>(new exa_real_t[(NUM_GHOST_LAYERS + 1) * maxNumPointsPerDim * maxNumPointsPerDim]);\n";

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

    //s += s"unsigned int maxNumPointsPerDim = (1u << (NUM_LEVELS - 1)) + 1 + 2 * NUM_GHOST_LAYERS;\n";
    for (neigh <- neighbors) {
      val neighDir = neigh._1;
      val neighName = neigh._2;

      //s += s"MPI_Request request_$neighName;\n";
      s += s"reqOutstanding_$neighName = false;\n";

      var size : String = "";
      //size += s"(NUM_GHOST_LAYERS + 1) * maxNumPointsPerDim * maxNumPointsPerDim";
      var sizeArray = new ListBuffer[String]();
      for (i <- (0 to 2))
        if (0 == neighDir(i))
          sizeArray += s"${Mapping.numPoints(9)}"; // FIXME: maxLevel
        else
          sizeArray += s"${Knowledge.numGhostLayers}";

      size += sizeArray.mkString(" * ");

      s += s"sendBuffer_$neighName = new exa_real_t[$size];\n";
      s += s"recvBuffer_$neighName = new exa_real_t[$size];\n";
      //s += s"sendBuffer_$neighName = boost::shared_array<exa_real_t>(new exa_real_t[$size]);\n";
      //s += s"recvBuffer_$neighName = boost::shared_array<exa_real_t>(new exa_real_t[$size]);\n";
    }

    s += s"\t}\n";

    return s;
  }
}

class WaitForMPIReq() {
  def toString_cpp : String = {
    var s : String = "";

    s += s"void waitForMPIReq (MPI_Request* request)\n{\n";
    s += s"MPI_Status stat;\n";
    s += s"if (MPI_ERR_IN_STATUS == MPI_Wait(request, &stat))\n{\n";
    s += s"char msg[MPI_MAX_ERROR_STRING];\n";
    s += s"int len;\n";
    s += s"MPI_Error_string(stat.MPI_ERROR, msg, &len);\n";
    s += "LOG_WARNING(\"MPI Error encountered (\" << msg << \")\");\n";
    s += s"}\n";
    s += s"*request = MPI_Request();\n";

    s += s"\t}\n";

    return s;
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

class CopyLocalToBuffer(field : String, buffer : String, indices : IndexRange) {
  def toString_cpp : String = {
    var s : String = "";
    s += "size_t entry = 0;\n";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indices.begin(i)}; ${dimToString(i)} <= ${indices.end(i)}; ++${dimToString(i)})\n";
    s += s"$buffer[entry++] = $field->data[${Mapping.access(indices.level)}];\n";
    //s += s"$buffer[entry++] = $field->data[z * $field->numDataPointsPerDimWPad.xy().componentProd() + y * $field->numDataPointsPerDimWPad.x + x];\n";

    return s;
  }
};

class CopyBufferToLocal(field : String, buffer : String, indices : IndexRange) {
  def toString_cpp : String = {
    var s : String = "";
    s += "size_t entry = 0;\n";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indices.begin(i)}; ${dimToString(i)} <= ${indices.end(i)}; ++${dimToString(i)})\n";
    s += s"$field->data[${Mapping.access(indices.level)}] = $buffer[entry++];\n";
    //s += s"$field->data[z * $field->numDataPointsPerDimWPad.xy().componentProd() + y * $field->numDataPointsPerDimWPad.x + x] = $buffer[entry++];\n";

    return s;
  }
};

class CopyLocalToLocal(fieldSrc : String, indicesSrc : IndexRange, fieldDest : String, indicesDest : IndexRange) {
  def toString_cpp : String = {
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
//  def toString_cpp : String = {
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

class CopyLocalToLocalHACK(fieldSrc : String, indicesSrc : IndexRange, fieldDest : String, indicesDest : IndexRange) {
  def toString_cpp : String = {
    var s : String = "";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indicesSrc.begin(i)}; ${dimToString(i)} <= ${indicesSrc.end(i)}; ++${dimToString(i)})\n";
    s += s"$fieldDest[${Mapping.access(indicesDest.level, s"(z - (${indicesSrc.begin(2)}) + (${indicesDest.begin(2)}))", s"(y - (${indicesSrc.begin(1)}) + (${indicesDest.begin(1)}))", s"(x - (${indicesSrc.begin(0)}) + (${indicesDest.begin(0)}))")}]" +
      s" = $fieldSrc[${Mapping.access(indicesSrc.level)}];\n";

    return s;
  }
};

class SetDataZero(field : String, indices : IndexRange) {
  def toString_cpp : String = {
    var s : String = "";

    for (i <- 2 to 0 by -1)
      s += s"for (unsigned int ${dimToString(i)} = ${indices.begin(i)}; ${dimToString(i)} <= ${indices.end(i)}; ++${dimToString(i)})\n";
    s += s"$field->data[${Mapping.access(indices.level)}] = 0.0;\n";
    //s += s"$field->data[z * $field->numDataPointsPerDimWPad.xy().componentProd() + y * $field->numDataPointsPerDimWPad.x + x] = 0.0;\n";

    return s;
  }
};

class SendBuffer(buffer : String, rank : String, tag : String, synchronous : Boolean = true, request : String = "request") {
  def toString_cpp : String = {
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

class RecvBuffer(buffer : String, rank : String, tag : String, size : String, synchronous : Boolean = true) {
  def toString_cpp : String = {
    var s : String = "";
    // TODO: calculate number of entries

    s += s"#pragma omp critical\n{\n";

    if (synchronous) {
      s += "NOT IMPLEMENTED YET\n";
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

class TreatNeighSend(field : Field, neighName : String, indicesLocal : IndexRange, indicesNeigh : IndexRange, level : Int) {
  def toString_cpp : String = {
    var s : String = "";
    s += (new ifCond(
      s"FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location",
      Array(
        s"FragmentNeighInfo& curNeigh = fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN];",
        (new ifCond(s"curNeigh.isRemote",
          Array(
            //"if (1 == mpiRank) LOG_NOTE(\"Sending from \" << fragments[e]->id << \" to \" << curNeigh.fragId);",
            (new CopyLocalToBuffer(s"fragments[e]->${field.codeName}[slot][$level]", s"fragments[e]->sendBuffer_$neighName", indicesLocal)).toString_cpp,
            (new SendBuffer(s"fragments[e]->sendBuffer_$neighName", s"curNeigh.remoteRank", s"((unsigned int)fragments[e]->id << 16) + ((unsigned int)curNeigh.fragId & 0x0000ffff)", true, s"fragments[e]->request_$neighName")).toString_cpp,
            s"fragments[e]->reqOutstanding_$neighName = true;"),
          Array(
            (new CopyLocalToLocal(s"fragments[e]->${field.codeName}[slot][$level]", indicesLocal, s"curNeigh.fragment->get${field.codeName}($level, slot)", indicesNeigh)).toString_cpp))).toString_cpp))).toString_cpp;
    return s;
  }
}

class TreatNeighSendRemote(field : Field, neighName : String, indicesLocal : IndexRange, indicesNeigh : IndexRange, level : Int) {
  def toString_cpp : String = {
    var s : String = "";
    s += (new ifCond(
      s"FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location && fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].isRemote",
      Array(
        s"FragmentNeighInfo& curNeigh = fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN];",
        //"if (1 == mpiRank) LOG_NOTE(\"Sending from \" << fragments[e]->id << \" to \" << curNeigh.fragId);",
        (new CopyLocalToBuffer(s"fragments[e]->${field.codeName}[slot][$level]", s"fragments[e]->sendBuffer_$neighName", indicesLocal)).toString_cpp,
        (new SendBuffer(s"fragments[e]->sendBuffer_$neighName", s"curNeigh.remoteRank", s"((unsigned int)fragments[e]->id << 16) + ((unsigned int)curNeigh.fragId & 0x0000ffff)", true, s"fragments[e]->request_$neighName")).toString_cpp,
        s"fragments[e]->reqOutstanding_$neighName = true;"))).toString_cpp;
    return s;
  }
}

class TreatNeighSendLocal(field : Field, neighName : String, indicesLocal : IndexRange, indicesNeigh : IndexRange, level : Int) {
  def toString_cpp : String = {
    var s : String = "";
    s += (new ifCond(
      s"isValid_$neighName",
      Array(
        (new CopyLocalToLocalHACK(s"localMem", indicesLocal, s"neighMem_$neighName", indicesNeigh)).toString_cpp))).toString_cpp;
    return s;
  }
}

class TreatNeighRecv(field : Field, neighName : String, indices : IndexRange, level : Int) {
  def toString_cpp : String = {
    var s : String = "";
    s += (new ifCond(
      s"FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location",
      Array(
        s"FragmentNeighInfo& curNeigh = fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN];",
        (new ifCond(s"curNeigh.isRemote",
          Array(
            //"if (1 == mpiRank) LOG_NOTE(\"Receiving from \" << curNeigh.fragId << \" to \" << fragments[e]->id);",
            (new RecvBuffer(s"fragments[e]->recvBuffer_$neighName", s"curNeigh.remoteRank", s"((unsigned int)curNeigh.fragId << 16) + ((unsigned int)fragments[e]->id & 0x0000ffff)",
              s"NUM_GHOST_LAYERS * fragments[e]->${field.codeName}[slot][$level]->numDataPointsPerDim.y * fragments[e]->${field.codeName}[slot][$level]->numDataPointsPerDim.z", false)).toString_cpp,
            (new CopyBufferToLocal(s"fragments[e]->${field.codeName}[slot][$level]", s"fragments[e]->recvBuffer_$neighName", indices)).toString_cpp))).toString_cpp))).toString_cpp;
    return s;
  }
}

class TreatNeighFinish(neighName : String) {
  def toString_cpp : String = {
    var s : String = "";

    s += (new ifCond(s"fragments[e]->reqOutstanding_$neighName",
      Array(
        s"#pragma omp critical",
        s"{",
        s"waitForMPIReq(&fragments[e]->request_$neighName);",
        s"}",
        s"fragments[e]->reqOutstanding_$neighName = false;"))).toString_cpp;

    return s;
  }
}

class TreatNeighBC(field : Field, neighName : String, indices : IndexRange, level : Int) {
  def toString_cpp : String = {
    var s : String = "";

    if (field.bcDir0) {
      s += (new ifCond(
        s"FRAG_INVALID == fragments[e]->neigh[FRAG_CUBE_$neighName - FRAG_CUBE_ZN_YN_XN].location",
        Array(
          (new SetDataZero(s"fragments[e]->${field.codeName}[slot][$level]", indices)).toString_cpp))).toString_cpp;
    }
    return s;
  }
}

class NeighInfo(var dir : Array[Int], level : Int) {
  var label : String = (2 to 0 by -1).toArray.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");
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

  var codeInit : String = "";
  var codeTreatBC : String = "";
  var codeExchLocal : String = "";

//  def addCodeInit(field : Field) {
//    codeInit += s"isValid_$label = (FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].location && !fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].isRemote);\n";
//    codeInit += s"neighMem_$label = isValid_$label ? fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].fragment->get${field.codeName}($level, slot)->data : 0;\n";
//  }

  def addTreatBC(field : Field) {
    codeTreatBC = "";
    if (field.bcDir0) {
      codeTreatBC += (new ifCond(
        s"FRAG_INVALID == fragments[e]->neigh[FRAG_CUBE_$label - FRAG_CUBE_ZN_YN_XN].location",
        Array(
          (new SetDataZero(s"fragments[e]->${field.codeName}[slot][$level]", indexBorder)).toString_cpp))).toString_cpp;
    }
  }

  def addExchLocal = {
    codeExchLocal = "";
    codeExchLocal += (new ifCond(
      s"isValid_$label",
      (2 to 0 by -1).toArray.map(i =>
        s"for (unsigned int ${dimToString(i)} = ${indexInner.begin(i)}; ${dimToString(i)} <= ${indexInner.end(i)}; ++${dimToString(i)})\n") ++
        Array(
          s"neighMem_$label[${Mapping.access(indexOpposingOuter.level, s"(z - (${indexInner.begin(2)}) + (${indexOpposingOuter.begin(2)}))", s"(y - (${indexInner.begin(1)}) + (${indexOpposingOuter.begin(1)}))", s"(x - (${indexInner.begin(0)}) + (${indexOpposingOuter.begin(0)}))")}]" +
            s" = localMem[${Mapping.access(indexInner.level)}];\n"))).toString_cpp;
  }
  
  // (new TreatNeighRecv(field, neigh.label, neigh.indexOuter, level)).toString_cpp).toArray)).toString_cpp;

}

class ExchangeDataSplitter(field : Field, maxLevel : Int) {
  def toString_cpp : String = {
    var s : String = "";

    s += s"void Fragment3DCube::exch${field.codeName} (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level, unsigned int slot /*= 0*/)\n\t{\n";
    s += s"switch (level)\n{\n";
    for (level <- (0 to maxLevel)) {
      s += s"case $level: exch${field.codeName}_$level(fragments, slot);\nbreak;\n";
    }
    s += s"}\n";
    s += s"\t}\n";

    return s;
  }
}

class ExchangeData(field : Field, level : Int) {
  def toString_cpp : String = {
    var s : String = "";

    s += s"void Fragment3DCube::exch${field.codeName}_$level (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int slot /*= 0*/)\n\t{\n";

    val fieldName = s"fragments[e]->${field.codeName}[slot][$level]";

    // NOTE: switch here
    if (false) {
      // simple exchange along axis
      val neighbors = Array(
        new NeighInfo(Array(-1, 0, 0), level), new NeighInfo(Array(1, 0, 0), level),
        new NeighInfo(Array(0, -1, 0), level), new NeighInfo(Array(0, 1, 0), level),
        new NeighInfo(Array(0, 0, -1), level), new NeighInfo(Array(0, 0, 1), level));

      for (neigh <- neighbors) {
        neigh.setIndicesWide(field);
      }

      // handle BC
      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        neighbors.map(neigh =>
          (new TreatNeighBC(field, neigh.label, fieldToIndexBorder(neigh.dir, fieldName, level), level)).toString_cpp).toArray)).toString_cpp;

      // sync duplicate values
      for (dim <- 0 to 2) {
        s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
          Array(
            (new TreatNeighSend(field, neighbors(2 * dim + 1).label,
              neighbors(2 * dim + 1).indexBorder,
              neighbors(2 * dim + 1).indexOpposingBorder, level)).toString_cpp))).toString_cpp;

        s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
          Array(
            (new TreatNeighRecv(field, neighbors(2 * dim + 0).label, neighbors(2 * dim + 0).indexBorder, level)).toString_cpp))).toString_cpp;

        s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
          Array(
            (new TreatNeighFinish(neighbors(2 * dim + 1).label)).toString_cpp))).toString_cpp;
      }

      // update ghost layers
      for (dim <- 0 to 2) {
        s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
          Array(0, 1).map(dir =>
            (new TreatNeighSend(field, neighbors(2 * dim + dir).label, neighbors(2 * dim + dir).indexInner,
              neighbors(2 * dim + dir).indexOpposingOuter, level)).toString_cpp).toArray)).toString_cpp;

        s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
          Array(0, 1).map(dir =>
            (new TreatNeighRecv(field, neighbors(2 * dim + dir).label, neighbors(2 * dim + dir).indexOuter, level)).toString_cpp).toArray)).toString_cpp;

        s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
          Array(0, 1).map(dir =>
            (new TreatNeighFinish(neighbors(2 * dim + dir).label)).toString_cpp).toArray)).toString_cpp;
      }
    } else {
      // 27 point communication
      val neighbors = new ListBuffer[NeighInfo]();//FragmentClass.neighbors;
      for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
        neighbors += new NeighInfo(Array(x, y, z), level);
      }

      for (neigh <- neighbors) {
        neigh.setIndices(field);
        neigh.addTreatBC(field);
        neigh.addExchLocal;
 
      }

      s += "int mpiRank; MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);\n";

      // handle BC
      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        //neighbors.map(neigh => neigh.codeTreatBC).toArray)).toString_cpp;
        FragmentClass.neighbors.map(neigh => neigh.getCode_TreatBC(field, level, "slot")).toArray)).toString_cpp;

      
      // sync duplicate values
      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh =>
          (new TreatNeighSend(field, neigh.label, neigh.indexBorder,
            neigh.indexOpposingBorder, level)).toString_cpp).toArray)).toString_cpp;

      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0).map(neigh =>
          (new TreatNeighRecv(field, neigh.label, neigh.indexBorder, level)).toString_cpp).toArray)).toString_cpp;

      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        neighbors.map(neigh =>
          (new TreatNeighFinish(neigh.label)).toString_cpp).toArray)).toString_cpp;

      // update ghost layers
      //      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
      //        neighbors.map(neigh =>
      //          (new TreatNeighSend(field, neigh.label, neigh.indexInner,
      //            neigh.indexOpposingOuter, level)).toString_cpp).toArray)).toString_cpp;
      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        neighbors.map(neigh =>
          (new TreatNeighSendRemote(field, neigh.label, neigh.indexInner,
            neigh.indexOpposingOuter, level)).toString_cpp).toArray)).toString_cpp;
      s += "//BEGIN LOCAL COMMUNICATION\n";
      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        Array(
          Array(s"exa_real_t* localMem = fragments[e]->${field.codeName}[slot][$level]->data;"),
          neighbors.map(neigh =>
            s"bool isValid_${neigh.label} = (FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_${neigh.label} - FRAG_CUBE_ZN_YN_XN].location && !fragments[e]->neigh[FRAG_CUBE_${neigh.label} - FRAG_CUBE_ZN_YN_XN].isRemote);").toArray,
          neighbors.map(neigh =>
            s"exa_real_t* neighMem_${neigh.label} = isValid_${neigh.label} ? fragments[e]->neigh[FRAG_CUBE_${neigh.label} - FRAG_CUBE_ZN_YN_XN].fragment->get${field.codeName}($level, slot)->data : 0;").toArray,
          neighbors.map(neigh => neigh.codeExchLocal).toArray).flatten)).toString_cpp;
      s += "//END LOCAL COMMUNICATION\n";

      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        neighbors.map(neigh =>
          (new TreatNeighRecv(field, neigh.label, neigh.indexOuter, level)).toString_cpp).toArray)).toString_cpp;

      s += (new forLoop(s"int e = 0; e < fragments.size(); ++e",
        neighbors.map(neigh =>
          (new TreatNeighFinish(neigh.label)).toString_cpp).toArray)).toString_cpp;
    }

    s += s"\t}\n";

    return s;
  }
}



object GenCommCode extends (() => Unit) {
  def apply() : Unit = {
    var fragment = new Fragment;

    println("Setting up Fragment");

    // HACK
    StateManager.root_ = fragment;

    StateManager.apply(new Transformation({
      case frag : Fragment =>
        frag.fields += new Field("Solution", "solData", "double", "NUM_SOL_SLOTS", true);
        frag.fields += new Field("Residual", "resData", "double", "1", false);
        frag.fields += new Field("RHS", "rhsData", "double", "1", false);
        Some(frag);
    }));

    StateManager.apply(new Transformation({
      case frag : Fragment =>
        frag.addSync(frag.fields);

        frag.functions += (new ConnectLocalElement().toString_cpp);
        frag.functions += (new ConnectRemoteElement().toString_cpp);
        frag.functions += (new SetupBuffers(frag.fields).toString_cpp);
        Some(frag);
    }));

    StateManager.apply(new Transformation({
      case frag : Fragment =>
        frag.toString_cpp;
        Some(frag);
    }));

    println("Setting up FragmentClass");

    // HACK
    StateManager.root_ = FragmentClass;

    StateManager.apply(new Transformation({
      case FragmentClass =>
        for (neigh <- FragmentClass.neighbors) {
          neigh.addDeclarations;
        }

        FragmentClass.toString_cpp;
        Some(FragmentClass);
    }));

    println("Done");
  }
}