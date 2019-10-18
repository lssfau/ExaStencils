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

package exastencils.operator.l4

import scala.collection.mutable.HashSet

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.stencil.ir.IR_StencilFunctionReference

/// L4_StencilFunctions

object L4_StencilFunctions {
  val functions = HashSet(
    "diag",
    "diag_inv", "diag_inverse")

  def getDatatype(fctName : String) =/* FIXME */ L4_UnknownDatatype
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_StencilFunctionReference

case class L4_StencilFunctionReference(var name : String, var returnType : L4_Datatype) extends L4_PlainFunctionReference {
  override def progress = ProgressLocation(IR_StencilFunctionReference(name, returnType.progress))
}

/// L4_ResolveStencilFunctions

object L4_ResolveStencilFunctions extends DefaultStrategy("Resolve stencil function references") {
  this += new Transformation("Resolve", {
    case L4_UnresolvedFunctionReference(fctName, level, offset) if L4_StencilFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled stencil function ${ fctName } with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found stencil function ${ fctName } with offset; offset is ignored")
      L4_StencilFunctionReference(fctName, L4_StencilFunctions.getDatatype(fctName))
  })
}
