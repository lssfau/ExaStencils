//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.globals.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config._
import exastencils.core._
import exastencils.prettyprinting._
import exastencils.timing.ir.IR_Stopwatch

/// IR_GlobalCollection

object IR_GlobalCollection extends ObjectWithState {
  def defBaseName = "Global/Global"
  def defHeader = defBaseName + ".h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_GlobalCollection] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_GlobalCollection]()
    selfRef.get
  }
}

case class IR_GlobalCollection(var variables : ListBuffer[IR_VariableDeclaration] = ListBuffer()) extends IR_FunctionCollection(IR_GlobalCollection.defBaseName,
  ListBuffer("algorithm"), // provides commonly used functions like min/max
  ListBuffer(),
  ListBuffer(
    IR_PlainFunction("initGlobals", IR_UnitDatatype),
    IR_PlainFunction("destroyGlobals", IR_UnitDatatype))) {

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
  internalDependencies += IR_Stopwatch.defHeader
  if (Knowledge.library_CImg)
    internalDependencies += "Util/CImg.h"

  def initGlobals = functions.find(_.name == "initGlobals").get
  def destroyGlobals = functions.find(_.name == "destroyGlobals").get

  override def printHeader() = {
    super.printHeader()
    val writer = PrettyprintingManager.getPrinter(s"${ baseName }.h")
    Settings.additionalMacros.foreach(writer <<< _)
    typeAliases.foreach(x => writer << s"using ${ x._1.prettyprint } = ${ x._2 };\n")
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

  var typeAliases = mutable.HashMap[IR_Datatype, String]()

  def registerTypeAlias(datatype : IR_Datatype, alias : String) = {
    typeAliases += ((datatype, alias))
  }
}
