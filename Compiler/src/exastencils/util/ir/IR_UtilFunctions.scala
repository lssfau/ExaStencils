package exastencils.util.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config._
import exastencils.core._
import exastencils.domain.ir.IR_ReadLineFromFile
import exastencils.prettyprinting._

/// IR_UtilFunctions

object IR_UtilFunctions extends ObjectWithState {
  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_UtilFunctions] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_UtilFunctions]()
    selfRef.get
  }
}

case class IR_UtilFunctions() extends IR_FunctionCollection("Util/Util",
  ListBuffer("algorithm"), // provides commonly used functions like min/max
  ListBuffer(),
  ListBuffer()) {

  // add conditional dependencies - TODO: move to respective packages
  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled) {
    externalDependencies += "iostream" // required for error messages
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"
  }
  if (Knowledge.data_alignFieldPointers || Knowledge.data_alignTmpBufferPointers)
    externalDependencies += "cstddef" // type ptrdiff_t (used when aligning pointer) is defined in stddef.h/cstddef
  internalDependencies += "Stopwatch.h"
  if (Knowledge.library_CImg)
    internalDependencies += "CImg.h"
  if (!Knowledge.experimental_internalHighDimTypes)
    internalDependencies += "Matrix.h"

  // read line from file
  var fctArgs : ListBuffer[IR_FunctionArgument] = ListBuffer()
  fctArgs += IR_FunctionArgument("ifs", IR_SpecialDatatype("std::ifstream&"))
  fctArgs += IR_FunctionArgument("iss", IR_SpecialDatatype("std::istringstream&"))

  if (Knowledge.domain_readFromFile)
    functions += IR_ReadLineFromFile(IR_ReadLineFromFile.arg1, IR_ReadLineFromFile.arg2)

  override def printHeader() = {
    super.printHeader()
    val writer = PrettyprintingManager.getPrinter(s"${ baseName }.h")
    Settings.additionalMacros.foreach(writer <<< _)
    typeAliases.foreach(x => writer << s"using ${ x._1.prettyprint } = ${ x._2 };\n")
  }

  override def printSources() = {
    val writer = PrettyprintingManager.getPrinter(s"${ baseName }_declarations.cpp")
    writer.addInternalDependency(s"${ baseName }.h")

    super.printSources()
  }

  var typeAliases = mutable.HashMap[IR_Datatype, String]()

  def registerTypeAlias(datatype : IR_Datatype, alias : String) = {
    typeAliases += ((datatype, alias))
  }
}
