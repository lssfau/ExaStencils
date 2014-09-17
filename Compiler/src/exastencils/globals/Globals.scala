package exastencils.globals

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class Globals(var variables : ListBuffer[VariableDeclarationStatement] = new ListBuffer) extends FunctionCollection("Globals/Globals",
  ListBuffer("#include \"Util/Vector.h\"")
    ++ (if (Knowledge.useMPI) ListBuffer("#pragma warning(disable : 4800)", "#include <mpi.h>") else ListBuffer())
    ++ (if (Knowledge.l3tmp_genAdvancedTimers) ListBuffer("#include \"Util/Stopwatch.h\"") else ListBuffer())
    ++ Settings.additionalIncludes,
  ListBuffer(
    new FunctionStatement(new UnitDatatype, "initGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]),
    new FunctionStatement(new UnitDatatype, "destroyGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]))) {

  override def printHeader = {
    val writer = PrettyprintingManager.getPrinter(s"${baseName}.h")

    for (inc <- includes) writer <<< inc
    for (variable <- variables) writer << s"extern ${variable.cpp_onlyDeclaration}\n"
    for (func <- functions) writer << func.asInstanceOf[FunctionStatement].cpp_decl
  }

  override def printSources = {
    val writer = PrettyprintingManager.getPrinter(s"${baseName}_declarations.cpp")

    writer <<< "#include \"" + baseName + ".h\""
    for (variable <- variables) writer << s"${variable.cpp()}\n"

    super.printSources
  }
}
