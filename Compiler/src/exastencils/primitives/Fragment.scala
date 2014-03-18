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
  className = "Fragment3DCube";

  var neighbors : ListBuffer[NeighborInfo] = ListBuffer();

  def init = {
    declarations += s"size_t id;";
    cTorInitList += s"id(-1)";

    declarations += s"Vec3 pos;";
    cTorInitList += s"pos(0.0, 0.0, 0.0)";

    if (6 == Knowledge.comm_strategyFragment) {
      neighbors += new NeighborInfo(Array(-1, 0, 0), 0);
      neighbors += new NeighborInfo(Array(+1, 0, 0), 1);
      if (Knowledge.dimensionality > 1) {
        neighbors += new NeighborInfo(Array(0, -1, 0), 2);
        neighbors += new NeighborInfo(Array(0, +1, 0), 3);
      }
      if (Knowledge.dimensionality > 2) {
        neighbors += new NeighborInfo(Array(0, 0, -1), 4);
        neighbors += new NeighborInfo(Array(0, 0, +1), 5);
      }
    } else if (26 == Knowledge.comm_strategyFragment) {
      var i = 0;
      for (
        z <- (if (Knowledge.dimensionality > 2) (-1 to 1) else (0 to 0));
        y <- (if (Knowledge.dimensionality > 1) (-1 to 1) else (0 to 0));
        x <- -1 to 1;
        if (0 != x || 0 != y || 0 != z)
      ) {
        neighbors += new NeighborInfo(Array(x, y, z), i);
        i += 1;
      }
    }

    var numNeighbors = neighbors.size;
    var cTorNeighLoopList = new ListBuffer[Statement];
    var dTorNeighLoopList = new ListBuffer[Statement];
    declarations += s"bool neighbor_isValid[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_isValid[i] = false;";
    declarations += s"bool neighbor_isRemote[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_isRemote[i] = false;";
    declarations += s"Fragment3DCube* neighbor_localPtr[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_localPtr[i] = NULL;";
    declarations += s"size_t neighbor_fragmentId[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_fragmentId[i] = -1;";
    declarations += s"int neighbor_remoteRank[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_remoteRank[i] = MPI_PROC_NULL;";

    for (sendOrRecv <- Array("Send", "Recv")) {
      declarations += StringLiteral(s"MPI_Request request_${sendOrRecv}[$numNeighbors];");
      declarations += StringLiteral(s"bool reqOutstanding_${sendOrRecv}[$numNeighbors];");
      cTorNeighLoopList += StringLiteral(s"reqOutstanding_${sendOrRecv}[i] = false;");

      declarations += StringLiteral(s"double* buffer_${sendOrRecv}[$numNeighbors];");
      cTorNeighLoopList += StringLiteral(s"buffer_${sendOrRecv}[i] = NULL;");
      dTorNeighLoopList += StringLiteral(s"if (buffer_${sendOrRecv}[i]) { delete [] buffer_${sendOrRecv}[i]; buffer_${sendOrRecv}[i] = 0; }");
    }

    declarations += StringLiteral(s"int maxElemRecvBuffer[$numNeighbors];");
    cTorNeighLoopList += StringLiteral(s"maxElemRecvBuffer[i] = 0;");

    cTorBody += new ForLoopStatement(s"unsigned int i = 0", s"i < $numNeighbors", s"++i",
      cTorNeighLoopList);
    dTorBody += new ForLoopStatement(s"unsigned int i = 0", s"i < $numNeighbors", s"++i",
      dTorNeighLoopList);
  }

  override def cpp = "NOT VALID ; CLASS = FragmentClass\n";

  override def printToFile = {
    {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/Fragment3DCube.h");

      writer << (
        "#pragma warning(disable : 4800)\n"
        + "#include <mpi.h>\n"
        + "#include \"Globals/Globals.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Container/Container.h\"\n");

      writer << super.cpp;
    }

    var i = 0;
    for (f <- functions) {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/Fragment3DCube_$i.cpp");

      writer << "#include \"Primitives/Fragment3DCube.h\"\n\n";
      writer << f.cpp + "\n";

      i += 1;
    }
  }
}

// FIXME: Think about moving all of this information to some other source. Maybe some kind of ... DSL ... or even Level4

case class ExchangeData_6(field : Field, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_6\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    var body = new ListBuffer[Statement];

    val fieldName = s"curFragment.${field.codeName}[slot][${field.level}]";

    // handle BC
    // FIXME: currently treats inner points
    body += new HandleBoundaries(field, neighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))))));

    // sync duplicate values
    for (dim <- 0 until Knowledge.dimensionality) {
      val sendRemoteData = ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val sendLocalData = ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxDupBegin
              case i if -neigh.dir(i) < 0  => field.layout(i).idxDupBegin
              case i if -neigh.dir(i) > 0  => field.layout(i).idxDupEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
              case i if -neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
              case i if -neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val recvRemoteData = ListBuffer(neighbors(2 * dim + 0)).map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));

      // TODO: group the next seven lines into a separate node?
      body += new CopyToSendBuffer(field, sendRemoteData);
      body += new RemoteSend(field, sendRemoteData);
      body += new LocalSend(field, sendLocalData);

      body += new RemoteReceive(field, recvRemoteData);
      body += new FinishRemoteRecv(neighbors);
      body += new CopyFromRecvBuffer(field, recvRemoteData);

      body += new FinishRemoteSend(neighbors);
    }

    // update ghost layers
    for (dim <- 0 until Knowledge.dimensionality) {
      var curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1));
      val sendRemoteData = curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxGhostBegin
            case i if neigh.dir(i) < 0  => field.layout(i).idxInner
            case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - field.layout(i).ghostEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxInner + field.layout(i).ghostBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val sendLocalData = curNeighbors.map(neigh => (neigh,
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxGhostBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxInner
              case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - field.layout(i).ghostEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
              case i if neigh.dir(i) < 0  => field.layout(i).idxInner + field.layout(i).ghostBegin - 1
              case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxGhostBegin
              case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostBegin
              case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
              case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostBegin + field.layout(i).ghostBegin - 1
              case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val recvRemoteData = curNeighbors.map(neigh => (neigh,
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxGhostBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxGhostBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxGhostEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
              case i if neigh.dir(i) < 0  => field.layout(i).idxGhostBegin + field.layout(i).ghostBegin - 1
              case i if neigh.dir(i) > 0  => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));

      body += new CopyToSendBuffer(field, sendRemoteData);
      body += new RemoteSend(field, sendRemoteData);
      body += new LocalSend(field, sendLocalData);

      body += new RemoteReceive(field, recvRemoteData);
      body += new FinishRemoteRecv(neighbors);
      body += new CopyFromRecvBuffer(field, recvRemoteData);

      body += new FinishRemoteSend(neighbors);
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_${field.level}",
      ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      body);
  }
}

case class ExchangeData_26(field : Field, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_26\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    var body = new ListBuffer[Statement];

    val fieldName = s"curFragment.${field.codeName}[slot][${field.level}]";

    // handle BC
    // FIXME: currently treats inner points
    body += new HandleBoundaries(field, neighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
        }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))))));

    // sync duplicate values
    {
      val sendRemoteData = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxInner
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxInner + field.layout(i).inner - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val sendLocalData = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxInner
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxInner + field.layout(i).inner - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxInner
              case i if -neigh.dir(i) < 0  => field.layout(i).idxDupBegin
              case i if -neigh.dir(i) > 0  => field.layout(i).idxDupEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxInner + field.layout(i).inner - 1
              case i if -neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
              case i if -neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val recvRemoteData = neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0).map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxInner
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxInner + field.layout(i).inner - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxDupBegin + field.layout(i).dupBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));

      body += new CopyToSendBuffer(field, sendRemoteData);
      body += new RemoteSend(field, sendRemoteData);
      body += new LocalSend(field, sendLocalData);

      body += new RemoteReceive(field, recvRemoteData);
      body += new FinishRemoteRecv(neighbors);
      body += new CopyFromRecvBuffer(field, recvRemoteData);

      body += new FinishRemoteSend(neighbors);
    }

    // update ghost layers
    {
      val sendRemoteData = neighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
            case i if neigh.dir(i) < 0  => field.layout(i).idxInner
            case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - field.layout(i).ghostEnd
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
        new MultiIndex(
          (0 until Knowledge.dimensionality).toArray.map(i => i match {
            case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
            case i if neigh.dir(i) < 0  => field.layout(i).idxInner + field.layout(i).ghostBegin - 1
            case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - 1
          }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val sendLocalData = neighbors.map(neigh => (neigh,
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxInner
              case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - field.layout(i).ghostEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
              case i if neigh.dir(i) < 0  => field.layout(i).idxInner + field.layout(i).ghostBegin - 1
              case i if neigh.dir(i) > 0  => field.layout(i).idxInner + field.layout(i).inner - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0))),
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxDupBegin
              case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostBegin
              case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if -neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
              case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostBegin + field.layout(i).ghostBegin - 1
              case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));
      val recvRemoteData = neighbors.map(neigh => (neigh,
        new IndexRange(
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxGhostBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxGhostEnd
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)),
          new MultiIndex(
            (0 until Knowledge.dimensionality).toArray.map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupEnd + field.layout(i).dupEnd - 1
              case i if neigh.dir(i) < 0  => field.layout(i).idxGhostBegin + field.layout(i).ghostBegin - 1
              case i if neigh.dir(i) > 0  => field.layout(i).idxGhostEnd + field.layout(i).ghostEnd - 1
            }) ++ (Knowledge.dimensionality until 3).toArray.map(i => 0)))));

      body += new CopyToSendBuffer(field, sendRemoteData);
      body += new RemoteSend(field, sendRemoteData);
      body += new LocalSend(field, sendLocalData);

      body += new RemoteReceive(field, recvRemoteData);
      body += new FinishRemoteRecv(neighbors);
      body += new CopyFromRecvBuffer(field, recvRemoteData);

      body += new FinishRemoteSend(neighbors);
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_${field.level}",
      ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      body);
  }
}
