package exastencils.globals

import scala.collection.mutable.ListBuffer

import exastencils.core.Settings
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class Globals(var variables : ListBuffer[VariableDeclarationStatement] = new ListBuffer) extends FunctionCollection("Globals/Globals",
  ListBuffer("algorithm"), // provides commonly used functions like min/max
  ListBuffer("Util/Vector.h", "Util/Matrix.h") /*
    ++ Settings.additionalIncludes*/ ,
  ListBuffer(
    new FunctionStatement(UnitDatatype, "initGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]),
    new FunctionStatement(UnitDatatype, "destroyGlobals", new ListBuffer[VariableAccess], new ListBuffer[Statement]))) {

  // add conditional dependencies
  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.data_alignFieldPointers || Knowledge.data_alignTmpBufferPointers)
    externalDependencies += "stddef.h" // TODO: please note why this is required; delete if it is not required
  internalDependencies += "Util/Stopwatch.h"
  if (Knowledge.library_CImg)
    internalDependencies += "Util/CImg.h"

  override def printHeader = {
    super.printHeader
    val writer = PrettyprintingManager.getPrinter(s"${baseName}.h")
    for (macroo <- Settings.additionalMacros) writer <<< macroo
    for (variable <- variables.sortBy(_.name)) writer << s"extern ${variable.prettyprint_onlyDeclaration}\n"
  }

  override def printSources = {
    val writer = PrettyprintingManager.getPrinter(s"${baseName}_declarations.cpp")
    writer.addInternalDependency(s"${baseName}.h")

    for (variable <- variables.sortBy(_.name)) writer << s"${variable.prettyprint()}\n"

    // additional include for std::srand
    PrettyprintingManager.getPrinter(s"${baseName}_initGlobals.cpp").addExternalDependency("cstdlib")

    super.printSources
  }
}
