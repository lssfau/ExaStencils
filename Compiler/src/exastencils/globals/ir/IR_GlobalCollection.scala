package exastencils.globals.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config._
import exastencils.core.StateManager
import exastencils.prettyprinting._

/// IR_GlobalCollection

object IR_GlobalCollection {
  // looks itself up starting from the current root
  def get = StateManager.findFirst[IR_GlobalCollection]().get
}

case class IR_GlobalCollection(var variables : ListBuffer[IR_VariableDeclaration] = ListBuffer()) extends IR_FunctionCollection("Globals/Globals",
  ListBuffer("algorithm"), // provides commonly used functions like min/max
  ListBuffer("Util/Matrix.h"),
  ListBuffer(
    IR_Function(IR_UnitDatatype, "initGlobals"),
    IR_Function(IR_UnitDatatype, "destroyGlobals"))) {

  // add conditional dependencies - TODO: move to respective packages
  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"
  }
  if (Knowledge.data_alignFieldPointers || Knowledge.data_alignTmpBufferPointers)
    externalDependencies += "cstddef" // type ptrdiff_t (used when aligning pointer) is defined in stddef.h/cstddef
  internalDependencies += "Util/Stopwatch.h"
  if (Knowledge.library_CImg)
    internalDependencies += "Util/CImg.h"

  def initGlobals = functions.find(_.name == "initGlobals").get
  def destroyGlobals = functions.find(_.name == "destroyGlobals").get

  override def printHeader() = {
    super.printHeader()
    val writer = PrettyprintingManager.getPrinter(s"${ baseName }.h")
    Settings.additionalMacros.foreach(writer <<< _)
    variables.sortBy(_.name).foreach(variable => writer << s"extern ${ variable.prettyprintDeclaration() }\n")
  }

  override def printSources() = {
    val writer = PrettyprintingManager.getPrinter(s"${ baseName }_declarations.cpp")
    writer.addInternalDependency(s"${ baseName }.h")

    variables.sortBy(_.name).foreach(variable => writer << s"${ variable.prettyprint() }\n")

    // additional include for std::srand
    PrettyprintingManager.getPrinter(s"${ baseName }_initGlobals.cpp").addExternalDependency("cstdlib")

    super.printSources()
  }
}
