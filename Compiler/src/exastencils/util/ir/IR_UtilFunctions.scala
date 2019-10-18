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

package exastencils.util.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config._
import exastencils.core._
import exastencils.prettyprinting._

/// IR_UtilFunctions

object IR_UtilFunctions extends ObjectWithState {
  def defBaseName = "Util/Util"
  def defHeader = defBaseName + ".h"

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

case class IR_UtilFunctions() extends IR_FunctionCollection(IR_UtilFunctions.defBaseName,
  ListBuffer(), // provides commonly used functions like min/max
  ListBuffer(),
  ListBuffer()) {

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
