package exastencils.globals

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class Globals(var variables : ListBuffer[VariableDeclarationStatement] = new ListBuffer) extends Node with FilePrettyPrintable {
  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer(
    new FunctionStatement(new UnitDatatype, "initGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]),
    new FunctionStatement(new UnitDatatype, "destroyGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]))

  override def printToFile = {
    val writerHeader = PrettyprintingManager.getPrinter(s"Globals/Globals.h")

    writerHeader << (
      "#include \"Util/Vector.h\"\n"
      + (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
      + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "") // FIXME: find a way to extract necessary includes from variables
      )

    if (Knowledge.testNewTimers)
      writerHeader <<< "#include \"Util/Stopwatch.h\""

    for (inc <- Settings.additionalIncludes)
      writerHeader <<< inc

    for (variable <- variables) { writerHeader << s"extern ${variable.cpp_onlyDeclaration}\n" }

    for (func <- functions)
      writerHeader << func.cpp_decl

    var i = 0;
    {
      val writerSource = PrettyprintingManager.getPrinter(s"Globals/Globals_$i.cpp")

      writerSource << "#include \"Globals/Globals.h\"\n\n"
      for (variable <- variables) { writerSource << s"${variable.cpp()}\n" }

      i += 1
    }

    for (func <- functions) {
      var s : String = ""

      val writerSource = PrettyprintingManager.getPrinter(s"Globals/Globals_$i.cpp")

      writerSource << "#include \"Globals/Globals.h\"\n\n"
      writerSource << func.cpp + "\n"

      i += 1
    }
  }
}
