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

case class Globals(var variables : ListBuffer[VariableDeclarationStatement] = new ListBuffer) extends Node with FilePrettyPrintable {
  var initFunction : FunctionStatement = new FunctionStatement(new UnitDatatype, "initGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement])

  override def printToFile = {
    val writerHeader = PrettyprintingManager.getPrinter(s"Globals/Globals.h")

    writerHeader << (
      "#include \"Util/Vector.h\"\n"
      + (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
      + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "") // FIXME: find a way to extract necessary includes from variables
      )

    writerHeader << "class Fragment3DCube;\n" // FIXME: find a way to extract necessary forward defines from variables

    for (variable <- variables) { writerHeader << s"extern ${variable.cpp}\n" }

    writerHeader << s"${initFunction.returntype.cpp} ${initFunction.name.cpp}(" + initFunction.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"

    val writerSource = PrettyprintingManager.getPrinter(s"Globals/Globals.cpp")

    writerSource << "#include \"Globals/Globals.h\"\n\n"
    for (variable <- variables) { writerSource << s"${variable.cpp}\n" }

    writerSource << initFunction.cpp + "\n"
  }
}
