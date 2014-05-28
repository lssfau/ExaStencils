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

  def init = {
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

    declarations += s"Vec3i iterationOffsetBegin"
    cTorInitList += s"iterationOffsetBegin(1, 1, 1)"
    declarations += s"Vec3i iterationOffsetEnd"
    cTorInitList += s"iterationOffsetEnd(-1, -1, -1)"

    if (6 == Knowledge.comm_strategyFragment) {
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
    } else if (26 == Knowledge.comm_strategyFragment) {
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

    val numDomains = StateManager.findFirst[DomainCollection]().get.domains.size

    declarations += s"bool isValidForSubdomain[$numDomains]"
    new ForLoopStatement(s"unsigned int d = 0", s"d < $numDomains", s"++d",
      "isValidForSubdomain[d] = false")

    var numNeighbors = neighbors.size
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

    if (Knowledge.domain_canHaveRemoteNeighs) {
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
    }

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
        "#define _USE_MATH_DEFINES\n"
        + "#include <cmath>\n"
        + (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
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

// FIXME: Think about moving all of this information to some other source. Maybe some kind of ... DSL ... or even Level4

case class ExchangeData_6(field : Field, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_6\n"

  override def expand : FunctionStatement = {
    var body = new ListBuffer[Statement]

    val fieldName = s"curFragment.${field.codeName.cpp}[slot]"

    // handle BC
    // FIXME: currently treats numInnerLayers points
    // FIXME: this treats more points than required (outer halo)
    body += new HandleBoundaries(field, neighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin //idxDupLeftBegin, idxGhostLeftBegin
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin //idxDupLeftBegin, idxGhostLeftBegin
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd //idxDupRightEnd, idxGhostRightEnd
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd //idxDupRightEnd, idxGhostRightEnd
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))))))

    // sync duplicate values
    if (field.communicatesDuplicated) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        for (dim <- 0 until Knowledge.dimensionality) {
          val sendRemoteData = ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
          val sendLocalData = ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
          val recvRemoteData = ListBuffer(neighbors(2 * dim + 0)).map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))

          if (Knowledge.domain_canHaveRemoteNeighs) {
            body += new CopyToSendBuffer(field, sendRemoteData)
            body += new RemoteSend(field, sendRemoteData)

            if (Knowledge.domain_canHaveLocalNeighs)
              body += new LocalSend(field, sendLocalData)

            body += new RemoteReceive(field, recvRemoteData)
            body += new FinishRemoteRecv(neighbors)
            body += new CopyFromRecvBuffer(field, recvRemoteData)

            body += new FinishRemoteSend(neighbors)
          } else if (Knowledge.domain_canHaveLocalNeighs) {
            body += new LocalSend(field, sendLocalData)
          }
        }
      }
    }

    // update ghost layers
    if (field.communicatesGhosts) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        for (dim <- 0 until Knowledge.dimensionality) {
          var curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1))
          val sendRemoteData = curNeighbors.map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
          val sendLocalData = curNeighbors.map(neigh => (neigh,
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                  case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
                  case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                  case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
                  case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
          val recvRemoteData = curNeighbors.map(neigh => (neigh,
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                  case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                  case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                  case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                  case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
                }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))

          if (Knowledge.domain_canHaveRemoteNeighs) {
            body += new CopyToSendBuffer(field, sendRemoteData)
            body += new RemoteSend(field, sendRemoteData)

            if (Knowledge.domain_canHaveLocalNeighs)
              body += new LocalSend(field, sendLocalData)

            body += new RemoteReceive(field, recvRemoteData)
            body += new FinishRemoteRecv(neighbors)
            body += new CopyFromRecvBuffer(field, recvRemoteData)

            body += new FinishRemoteSend(neighbors)
          } else if (Knowledge.domain_canHaveLocalNeighs) {
            body += new LocalSend(field, sendLocalData)
          }
        }
      }
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName.cpp}",
      ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      body)
  }
}

case class ExchangeData_26(field : Field, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_26\n"

  override def expand : FunctionStatement = {
    var body = new ListBuffer[Statement]

    val fieldName = s"curFragment.${field.codeName.cpp}[slot]"

    // handle BC
    // FIXME: currently treats numInnerLayers points
    body += new HandleBoundaries(field, neighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))))))

    // sync duplicate values
    if (field.communicatesDuplicated) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        val sendRemoteData = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
        val sendLocalData = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
                case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
                case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
        val recvRemoteData = neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0).map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))

        if (Knowledge.domain_canHaveRemoteNeighs) {
          body += new CopyToSendBuffer(field, sendRemoteData)
          body += new RemoteSend(field, sendRemoteData)

          if (Knowledge.domain_canHaveLocalNeighs)
            body += new LocalSend(field, sendLocalData)

          body += new RemoteReceive(field, recvRemoteData)
          body += new FinishRemoteRecv(neighbors)
          body += new CopyFromRecvBuffer(field, recvRemoteData)

          body += new FinishRemoteSend(neighbors)
        } else if (Knowledge.domain_canHaveLocalNeighs) {
          body += new LocalSend(field, sendLocalData)
        }
      }
    }

    // update ghost layers
    if (field.communicatesGhosts) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        val sendRemoteData = neighbors.map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
              case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
        val sendLocalData = neighbors.map(neigh => (neigh,
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))
        val recvRemoteData = neighbors.map(neigh => (neigh,
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
              }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))))

        if (Knowledge.domain_canHaveRemoteNeighs) {
          body += new CopyToSendBuffer(field, sendRemoteData)
          body += new RemoteSend(field, sendRemoteData)

          if (Knowledge.domain_canHaveLocalNeighs)
            body += new LocalSend(field, sendLocalData)

          body += new RemoteReceive(field, recvRemoteData)
          body += new FinishRemoteRecv(neighbors)
          body += new CopyFromRecvBuffer(field, recvRemoteData)

          body += new FinishRemoteSend(neighbors)
        } else if (Knowledge.domain_canHaveLocalNeighs) {
          body += new LocalSend(field, sendLocalData)
        }
      }
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName.cpp}",
      ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      body)
  }
}
