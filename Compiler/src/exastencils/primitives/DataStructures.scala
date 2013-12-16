package exastencils.primitives

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.l4.VariableDeclarationStatement

trait Expandable {
  def expand() : Node
}

case class Field(name : String, codeName : String, dataType : String, numSlots : String, bcDir0 : Boolean) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class LoopOverDimensions(var indices : IndexRange, var body : ListBuffer[Statement]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(indices : IndexRange, body : Statement) = this(indices, ListBuffer[Statement](body));

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n";

  def expand : ForLoopStatement = {
    new ForLoopStatement(s"unsigned int ${dimToString(2)} = ${indices.begin(2)}", s"${dimToString(2)} <= ${indices.end(2)}", s"++${dimToString(2)}",
      new ForLoopStatement(s"unsigned int ${dimToString(1)} = ${indices.begin(1)}", s"${dimToString(1)} <= ${indices.end(1)}", s"++${dimToString(1)}",
        new ForLoopStatement(s"unsigned int ${dimToString(0)} = ${indices.begin(0)}", s"${dimToString(0)} <= ${indices.end(0)}", s"++${dimToString(0)}",
          body)));
  }
}

case class FieldAccess(var field : Field, var level : Expression, var slot : Expression, var index : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = {
    s"curFragment.${field.codeName}[${slot.cpp}][${level.cpp}]->data[${index.cpp}]";
  }
}

case class LocalNeighborFieldAccess(var neighborPtr : Expression, var field : Field, var level : Expression, var slot : Expression, var index : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = {
    s"${neighborPtr.cpp}->${field.codeName}[${slot.cpp}][${level.cpp}]->data[${index.cpp}]";
  }
}

case class LoopOverFragments(var body : ListBuffer[Statement]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(body : Statement) = this(ListBuffer(body));

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n";

  def expand : StatementBlock = {
    new StatementBlock(
      ListBuffer[Statement](
        "#pragma omp parallel for schedule(static, 1)", // FIXME: move to own Node
        ForLoopStatement(s"int e = 0", s"e < fragments.size()", s"++e",
          ListBuffer[Statement]("Fragment3DCube& curFragment = *(fragments[e].get());")
            ++ body)));
  }
}

abstract class Class extends Statement {
  var className : String = "CLASS_NAME";
  var declarations : ListBuffer[Statement] = ListBuffer();
  // FIXME: use specialized c'tor and d'tor nodes
  var cTorArgs : ListBuffer[Expression] = ListBuffer();
  var cTorInitList : ListBuffer[Expression] = ListBuffer();
  var cTorBody : ListBuffer[Statement] = ListBuffer();
  var dTorBody : ListBuffer[Statement] = ListBuffer();
  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer();

  def cpp : String = {
    var s : String = "";

    s += s"class $className\n{\n";

    s += s"public:\n";

    for (decl <- declarations)
      s += s"${decl.cpp}\n";

    s += s"$className (${cTorArgs.map(stat => stat.cpp).mkString(", ")})\n:\n";
    s += cTorInitList.map(stat => stat.cpp).mkString(",\n");
    s += s"{\n"
    for (stat <- cTorBody)
      s += s"${stat.cpp}\n";
    s += s"}\n";

    s += s"~$className ()\n";
    s += s"{\n"
    for (stat <- dTorBody)
      s += s"${stat.cpp}\n";
    s += s"}\n";

    for (func <- functions) {
      val function = func.asInstanceOf[FunctionStatement];
      s += s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n";
    }

    s += s"};\n";

    return s;
  }
}

case class FragmentClass extends Class with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  className = "Fragment3DCube";

  var neighbors : ListBuffer[NeighInfo] = ListBuffer();

  def init = {
    declarations += s"exa_id_t id;";
    declarations += s"Vec3 pos;";
    // FIXME: set these parameters not via constructor but afterwards directly
    cTorInitList += s"id(id)";
    cTorInitList += s"pos(pos)";
    cTorArgs += s"exa_id_t id"; // FIXME: specialized nodes...
    cTorArgs += s"const Vec3& pos"; // FIXME: specialized nodes...

    if (6 == Knowledge.fragmentCommStrategy) {
      neighbors += new NeighInfo(Array(-1, 0, 0), -1 /*FIXME*/ , 12);
      neighbors += new NeighInfo(Array(+1, 0, 0), -1 /*FIXME*/ , 14);
      neighbors += new NeighInfo(Array(0, -1, 0), -1 /*FIXME*/ , 10);
      neighbors += new NeighInfo(Array(0, +1, 0), -1 /*FIXME*/ , 16);
      neighbors += new NeighInfo(Array(0, 0, -1), -1 /*FIXME*/ , 4);
      neighbors += new NeighInfo(Array(0, 0, +1), -1 /*FIXME*/ , 22);
    } else if (26 == Knowledge.fragmentCommStrategy) {
      for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
        neighbors += new NeighInfo(Array(x, y, z), -1 /*FIXME*/ , (z + 1) * 9 + (y + 1) * 3 + (x + 1));
      }
    }

    var numNeighbors = 27; // FIXME: use actual number of neighbors
    var cTorNeighLoopList = new ListBuffer[Statement];
    var dTorNeighLoopList = new ListBuffer[Statement];
    declarations += s"bool neighbor_isValid[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_isValid[i] = false;";
    declarations += s"bool neighbor_isRemote[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_isRemote[i] = false;";
    declarations += s"Fragment3DCube* neighbor_localPtr[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_localPtr[i] = NULL;";
    declarations += s"exa_id_t neighbor_fragmentId[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_fragmentId[i] = -1;";
    declarations += s"int neighbor_remoteRank[$numNeighbors];";
    cTorNeighLoopList += s"neighbor_remoteRank[i] = MPI_PROC_NULL;";

    for (sendOrRecv <- Array("Send", "Recv")) {
      declarations += StringLiteral(s"MPI_Request request_${sendOrRecv}[$numNeighbors];");
      declarations += StringLiteral(s"bool reqOutstanding_${sendOrRecv}[$numNeighbors];");
      cTorNeighLoopList += StringLiteral(s"reqOutstanding_${sendOrRecv}[i] = false;");

      declarations += StringLiteral(s"exa_real_t* buffer_${sendOrRecv}[$numNeighbors];");
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
      val writer = new PrintWriter(new File(Globals.printPath + s"Primitives/Fragment3DCube.h"));

      writer.write(
        "#ifndef	PRIMITIVES_FRAGMENT3DCUBE_H\n"
          + "#define	PRIMITIVES_FRAGMENT3DCUBE_H\n"
          + "#include <vector>\n"
          + "#pragma warning(disable : 4800)\n"
          + "#include <mpi.h>\n"
          + "#include <boost/smart_ptr/shared_array.hpp>\n"
          + "#include \"Util/StdIncludes.h\"\n"
          + "#include \"Util/Vector.h\"\n"
          + "#include \"Container/Container.h\"\n"
          + "#include \"Primitives/CommunicationFunctions.h\"\n");

      writer.write(super.cpp);

      writer.write("#endif\n");

      writer.close();
    }

    var i = 0;
    for (f <- functions) {
      var s : String = "";

      s += "#include \"Primitives/Fragment3DCube.h\"\n\n";

      s += s"${f.cpp}\n";

      val writer = new PrintWriter(new File(Globals.printPath + s"Primitives/Fragment3DCube_$i.cpp"));
      writer.write(s);
      writer.close();

      i += 1;
    }
  }
}

case class CommunicationFunctions extends Statement with FilePrettyPrintable /*FIXME: Statement is required for root*/ {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer();

  override def cpp = "NOT VALID ; CLASS = CommunicationFunctions\n";

  override def printToFile = {
    {
      val writer = new PrintWriter(new File(Globals.printPath + s"Primitives/CommunicationFunctions.h"));

      writer.write(
        "#ifndef	COMMUNICATION_FUNCTIONS_H\n"
          + "#define	COMMUNICATION_FUNCTIONS_H\n"
          + "#pragma warning(disable : 4800)\n"
          + "#include <mpi.h>\n"
          + "#include \"Util/StdIncludes.h\"\n"
          + "#include \"Util/Vector.h\"\n"
          + "#include \"Container/Container.h\"\n"
          + "#include \"Primitives/Fragment3DCube.h\"\n");

      for (func <- functions) {
        val function = func.asInstanceOf[FunctionStatement];
        writer.write(s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n");
      }

      writer.write("#endif\n");

      writer.close();
    }

    var i = 0;
    for (f <- functions) {
      var s : String = "";

      s += "#include \"Primitives/CommunicationFunctions.h\"\n\n";

      s += s"${f.cpp}\n";

      val writer = new PrintWriter(new File(Globals.printPath + s"Primitives/CommunicationFunction_$i.cpp"));
      writer.write(s);
      writer.close();

      i += 1;
    }
  }
}

case class FieldCollection extends Statement /*FIXME: Statement is required for root*/ {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var fields : ListBuffer[Field] = ListBuffer();

  def cpp = "NOT VALID ; CLASS = FieldCollection\n";
}