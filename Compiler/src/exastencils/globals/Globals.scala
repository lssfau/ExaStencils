package exastencils.globals

import scala.collection.mutable.ListBuffer

import java.io.PrintWriter
import java.io.File

import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._

case class Globals() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var variables : ListBuffer[VariableDeclarationStatement] = new ListBuffer;
  var defines : ListBuffer[DefineStatement] = new ListBuffer; // FIXME: defines should be resolved automatically; currently this is required as an interface to Harald's prototype
  var initFunction : FunctionStatement = new FunctionStatement(new UnitDatatype, "initGlobals", new ListBuffer[Variable], new ListBuffer[Statement])

  override def printToFile = {
    val writerHeader = PrettyprintingManager.getPrinter(s"Globals/Globals.h");

    writerHeader << ("#ifndef	GLOBALS_H\n"
      + "#define	GLOBALS_H\n"
      + "#include \"Util/Vector.h\"\n"
      + "#pragma warning(disable : 4800)\n"
      + "#include <mpi.h>\n" // FIXME: find a way to extract necessary includes from variables
      );

    for (define <- defines) { writerHeader << s"${define.cpp}\n"; }

    writerHeader << "class Fragment3DCube;\n"; // FIXME: find a way to extract necessary forward defines from variables
    writerHeader << "template<class T> class MyArray;\n";
    writerHeader << "template<class T> class MyStencil;\n";

    for (variable <- variables) { writerHeader << s"extern ${variable.cpp}\n"; }

    writerHeader << s"${initFunction.returntype.cpp} ${initFunction.name}(" + initFunction.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n";

    writerHeader << "#endif\n";

    val writerSource = PrettyprintingManager.getPrinter(s"Globals/Globals.cpp");

    writerSource << "#include \"Globals/Globals.h\"\n\n";
    for (variable <- variables) { writerSource << s"${variable.cpp}\n"; }

    writerSource << initFunction.cpp + "\n";
  }
}
