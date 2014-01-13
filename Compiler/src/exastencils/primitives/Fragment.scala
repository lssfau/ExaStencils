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
  override def duplicate = this.copy().asInstanceOf[this.type]

  className = "Fragment3DCube";

  var neighbors : ListBuffer[NeighborInfo] = ListBuffer();

  def init = {
    declarations += s"size_t id;";
    cTorInitList += s"id(-1)";

    declarations += s"Vec3 pos;";
    cTorInitList += s"pos(0.0, 0.0, 0.0)";

    if (6 == Knowledge.fragmentCommStrategy) {
      neighbors += new NeighborInfo(Array(-1, 0, 0), 0);
      neighbors += new NeighborInfo(Array(+1, 0, 0), 1);
      neighbors += new NeighborInfo(Array(0, -1, 0), 2);
      neighbors += new NeighborInfo(Array(0, +1, 0), 3);
      neighbors += new NeighborInfo(Array(0, 0, -1), 4);
      neighbors += new NeighborInfo(Array(0, 0, +1), 5);
    } else if (26 == Knowledge.fragmentCommStrategy) {
      var i = 0;
      for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
        neighbors += new NeighborInfo(Array(x, y, z), i);
        i += 1;
      }
    }

    var numNeighbors = Knowledge.fragmentCommStrategy;
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
        "#ifndef	PRIMITIVES_FRAGMENT3DCUBE_H\n"
        + "#define	PRIMITIVES_FRAGMENT3DCUBE_H\n"
        + "#pragma warning(disable : 4800)\n"
        + "#include <mpi.h>\n"
        + "#include \"Globals/Globals.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Container/Container.h\"\n");

      writer << super.cpp;

      writer << "#endif\n";

      writer.close(); // FIXME: finalize
    }

    var i = 0;
    for (f <- functions) {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/Fragment3DCube_$i.cpp");

      writer << "#include \"Primitives/Fragment3DCube.h\"\n\n";
      writer << f.cpp + "\n";

      writer.close(); // FIXME: finalize

      i += 1;
    }
  }
}

case class ExchangeData_6(field : Field, level : Integer, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_6\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    var body = new ListBuffer[Statement];

    val fieldName = s"curFragment.${field.codeName}[slot][$level]";

    for (neigh <- neighbors) {
      neigh.setIndicesWide(field, level);
    }

    // handle BC
    body += new HandleBoundaries(field, level,
      neighbors.map(neigh => (neigh, neigh.indexBorder)));

    // sync duplicate values
    for (dim <- 0 to 2) {
      body += new CopyToSendBuffer_and_RemoteSend(field, level,
        ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, neigh.indexBorder)));
      body += new LocalSend(field, level,
        ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, neigh.indexBorder, neigh.indexOpposingBorder)));

      body += new RemoteReceive(field, level, ListBuffer(neighbors(2 * dim + 0)));

      body += new FinishRemoteCommunication(neighbors);

      body += new CopyFromRecvBuffer(field, level,
        ListBuffer(neighbors(2 * dim + 0)).map(neigh => (neigh, neigh.indexBorder)));
    }

    // update ghost layers
    for (dim <- 0 to 2) {
      var curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1));

      body += new CopyToSendBuffer_and_RemoteSend(field, level,
        curNeighbors.map(neigh => (neigh, neigh.indexInner)));

      body += new LocalSend(field, level,
        curNeighbors.map(neigh => (neigh, neigh.indexInner, neigh.indexOpposingOuter)));

      body += new RemoteReceive(field, level, curNeighbors);

      body += new FinishRemoteCommunication(curNeighbors);

      body += new CopyFromRecvBuffer(field, level,
        curNeighbors.map(neigh => (neigh, neigh.indexOuter)));
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_$level",
      ListBuffer(Variable("unsigned int", "slot")),
      body);
  }
}

case class ExchangeData_26(field : Field, level : Integer, neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_26\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    var body = new ListBuffer[Statement];

    val fieldName = s"curFragment.${field.codeName}[slot][$level]";

    for (neigh <- neighbors) {
      neigh.setIndices(field, level);
    }

    // handle BC
    body += new HandleBoundaries(field, level,
      neighbors.map(neigh => (neigh, neigh.indexBorder)));

    // sync duplicate values
    body += new CopyToSendBuffer_and_RemoteSend(field, level,
      neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, neigh.indexBorder)));
    body += new LocalSend(field, level,
      neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, neigh.indexBorder, neigh.indexOpposingBorder)));

    body += new RemoteReceive(field, level, neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0));

    body += new FinishRemoteCommunication(neighbors);

    body += new CopyFromRecvBuffer(field, level,
      neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0).map(neigh => (neigh, neigh.indexBorder)));

    // update ghost layers
    body += new CopyToSendBuffer_and_RemoteSend(field, level,
      neighbors.map(neigh => (neigh, neigh.indexInner)));
    body += new LocalSend(field, level,
      neighbors.map(neigh => (neigh, neigh.indexInner, neigh.indexOpposingOuter)));

    body += new RemoteReceive(field, level, neighbors);

    body += new FinishRemoteCommunication(neighbors);

    body += new CopyFromRecvBuffer(field, level,
      neighbors.map(neigh => (neigh, neigh.indexOuter)));

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${field.codeName}_$level",
      ListBuffer(Variable("unsigned int", "slot")),
      body);
  }
}
