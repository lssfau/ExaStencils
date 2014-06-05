package exastencils.primitives

import java.io.PrintWriter
import java.io.File

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.prettyprinting._

case class FragmentClass() extends Class with FilePrettyPrintable {
  className = "Fragment3DCube"

  var neighbors : ListBuffer[NeighborInfo] = ListBuffer()

  def setupNeighbors() : Unit = {
    Knowledge.comm_strategyFragment match {
      case 6 => {
        neighbors += new NeighborInfo(Array(-1, 0, 0), 0)
        neighbors += new NeighborInfo(Array(+1, 0, 0), 1)
        if (Knowledge.dimensionality > 1) {
          neighbors += new NeighborInfo(Array(0, -1, 0), 2)
          neighbors += new NeighborInfo(Array(0, +1, 0), 3)
        }
        if (Knowledge.dimensionality > 2) {
          neighbors += new NeighborInfo(Array(0, 0, -1), 4)
          neighbors += new NeighborInfo(Array(0, 0, +1), 5)
        }
      }
      case 26 => {
        var i = 0
        for (
          z <- (if (Knowledge.dimensionality > 2) (-1 to 1) else (0 to 0));
          y <- (if (Knowledge.dimensionality > 1) (-1 to 1) else (0 to 0));
          x <- -1 to 1;
          if (0 != x || 0 != y || 0 != z)
        ) {
          neighbors += new NeighborInfo(Array(x, y, z), i)
          i += 1
        }
      }
    }
  }

  def setupDefaultMembers() : Unit = {
    val numDomains = StateManager.findFirst[DomainCollection]().get.domains.size

    // TODO: employ variable name manager
    declarations += s"size_t id"
    cTorInitList += s"id(-1)"
    declarations += s"size_t commId"
    cTorInitList += s"commId(-1)"

    declarations += s"Vec3 pos"
    cTorInitList += s"pos(0.0, 0.0, 0.0)"
    declarations += s"Vec3 posBegin"
    cTorInitList += s"posBegin(0.0, 0.0, 0.0)"
    declarations += s"Vec3 posEnd"
    cTorInitList += s"posEnd(0.0, 0.0, 0.0)"

    declarations += s"Vec3i iterationOffsetBegin[$numDomains]"
    declarations += s"Vec3i iterationOffsetEnd[$numDomains]"

    cTorBody += new ForLoopStatement(s"unsigned int d = 0", s"d < $numDomains", s"++d",
      ListBuffer[Statement](
        "iterationOffsetBegin[d] = Vec3i(1, 1, 1)",
        "iterationOffsetEnd[d] = Vec3i(-1, -1, -1)"))
  }

  def setupBasicNeighborhoodMembers() : Unit = {
    val numDomains = StateManager.findFirst[DomainCollection]().get.domains.size
    val numNeighbors = neighbors.size

    declarations += s"bool isValidForSubdomain[$numDomains]"
    dTorBody += new ForLoopStatement(s"unsigned int d = 0", s"d < $numDomains", s"++d",
      "isValidForSubdomain[d] = false")

    var cTorNeighLoopList = new ListBuffer[Statement]
    var dTorNeighLoopList = new ListBuffer[Statement]
    declarations += s"bool neighbor_isValid[$numDomains][$numNeighbors]"
    cTorNeighLoopList += s"neighbor_isValid[d][i] = false"
    declarations += s"bool neighbor_isRemote[$numDomains][$numNeighbors]"
    cTorNeighLoopList += s"neighbor_isRemote[d][i] = false"
    declarations += s"Fragment3DCube* neighbor_localPtr[$numDomains][$numNeighbors]"
    cTorNeighLoopList += s"neighbor_localPtr[d][i] = NULL"
    declarations += s"size_t neighbor_fragCommId[$numDomains][$numNeighbors]"
    cTorNeighLoopList += s"neighbor_fragCommId[d][i] = -1"

    cTorBody += new ForLoopStatement(s"unsigned int d = 0", s"d < $numDomains", s"++d",
      new ForLoopStatement(s"unsigned int i = 0", s"i < $numNeighbors", s"++i",
        cTorNeighLoopList))
    dTorBody += new ForLoopStatement(s"unsigned int i = 0", s"i < $numNeighbors", s"++i",
      dTorNeighLoopList)
  }

  def setupRemoteNeighborhoodMembers : Unit = {
    val numDomains = StateManager.findFirst[DomainCollection]().get.domains.size
    val numNeighbors = neighbors.size

    var cTorNeighLoopList = new ListBuffer[Statement]
    var dTorNeighLoopList = new ListBuffer[Statement]

    declarations += s"int neighbor_remoteRank[$numDomains][$numNeighbors]"
    cTorNeighLoopList += s"neighbor_remoteRank[d][i] = MPI_PROC_NULL"

    for (sendOrRecv <- Array("Send", "Recv")) {
      declarations += s"MPI_Request request_${sendOrRecv}[$numNeighbors]"
      declarations += s"bool reqOutstanding_${sendOrRecv}[$numNeighbors]"
      cTorNeighLoopList += s"reqOutstanding_${sendOrRecv}[i] = false"

      declarations += s"double* buffer_${sendOrRecv}[$numNeighbors]"
      cTorNeighLoopList += s"buffer_${sendOrRecv}[i] = NULL"
      dTorNeighLoopList += s"if (buffer_${sendOrRecv}[i]) { delete [] buffer_${sendOrRecv}[i]; buffer_${sendOrRecv}[i] = 0; }"
    }

    declarations += s"int maxElemRecvBuffer[$numNeighbors]"
    cTorNeighLoopList += s"maxElemRecvBuffer[i] = 0"

    cTorBody += new ForLoopStatement(s"unsigned int d = 0", s"d < $numDomains", s"++d",
      new ForLoopStatement(s"unsigned int i = 0", s"i < $numNeighbors", s"++i",
        cTorNeighLoopList))
    dTorBody += new ForLoopStatement(s"unsigned int i = 0", s"i < $numNeighbors", s"++i",
      dTorNeighLoopList)
  }

  override def cpp = "NOT VALID ; CLASS = FragmentClass\n"

  override def printToFile = {
    {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/Fragment3DCube.h")

      writer << (
        (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
        + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "")
        + "#include \"Globals/Globals.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n")

      writer << super.cpp
    }

    var i = 0
    for (f <- functions) {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/Fragment3DCube_$i.cpp")

      writer << "#include \"Primitives/Fragment3DCube.h\"\n\n"
      writer << f.cpp + "\n"

      i += 1
    }
  }
}
