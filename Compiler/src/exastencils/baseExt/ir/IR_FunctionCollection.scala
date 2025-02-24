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

package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.prettyprinting._

/// IR_FunctionCollection

abstract class IR_FunctionCollection(var baseName : String,
    var externalDependencies : ListBuffer[String],
    var internalDependencies : ListBuffer[String],
    var functions : ListBuffer[IR_FunctionLike] = ListBuffer()) extends IR_Node with FilePrettyPrintable {

  /// add a function to the function collection
  def +=(fct : IR_FunctionLike) = {
    functions += fct
    this
  }

  def -=(fct : IR_FunctionLike) = {
    functions -= fct
    this
  }

  def printHeader() = {
    val writer = PrettyprintingManager.getPrinter(s"$baseName.h")
    for (inc <- internalDependencies)
      writer.addInternalDependency(inc)
    for (inc <- externalDependencies)
      writer.addExternalDependency(inc)

    val externC = Knowledge.generateFortranInterface || Knowledge.generateCInterface

    // header only functions
    for (func <- functions)
      if (func.isHeaderOnly)
        writer <<< func.prettyprint

    if (externC)
      writer <<< "extern \"C\" {"

    // functions with separate definition
    for (func <- functions)
      if (!func.isHeaderOnly && !func.hasAnnotation("deviceOnly"))
        writer << func.prettyprint_decl

    if (externC)
      writer <<< "}"
  }

  def printSources() = {
    // will be overwritten for kernel functions
    for (func <- functions)
      if (!func.isHeaderOnly) {
        val writer = PrettyprintingManager.getPrinter(s"${ baseName }_${ func.name }.cpp")
        writer.addInternalDependency(s"$baseName.h")

        writer <<< func.prettyprint
      }
  }

  override def printToFile() = {
    functions = functions.sortBy({
      case fs : IR_Function => fs.name
      case _                => ""
    })

    printHeader()
    printSources()
  }
}
