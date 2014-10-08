package exastencils.globals

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class Globals(var variables : ListBuffer[VariableDeclarationStatement] = new ListBuffer) extends FunctionCollection("Globals/Globals",
  (if (Knowledge.useMPI) ListBuffer("mpi.h") else ListBuffer()),
  ListBuffer("Util/Vector.h")
    ++ (if (Knowledge.l3tmp_genAdvancedTimers) ListBuffer("Util/Stopwatch.h") else ListBuffer()) /*
    ++ Settings.additionalIncludes*/ ,
  ListBuffer(
    new FunctionStatement(new UnitDatatype, "initGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]),
    new FunctionStatement(new UnitDatatype, "destroyGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]))) {

  override def printHeader = {
    super.printHeader
    val writer = PrettyprintingManager.getPrinter(s"${baseName}.h")
    for (variable <- variables) writer << s"extern ${variable.prettyprint_onlyDeclaration}\n"
  }

  override def printSources = {
    val writer = PrettyprintingManager.getPrinter(s"${baseName}_declarations.cpp")
    writer.addInternalDependency(s"${baseName}.h")

    for (variable <- variables) writer << s"${variable.prettyprint()}\n"

    super.printSources
  }
}
