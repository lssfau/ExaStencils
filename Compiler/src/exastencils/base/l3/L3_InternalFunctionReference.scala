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

package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._

/// L3_InternalFunctionReference

object L3_InternalFunctionReference {
  def floord = L3_PlainInternalFunctionReference("floord", L3_RealDatatype/* TODO: check data type */)
}

trait L3_InternalFunctionReference extends L3_FunctionReference

/// L3_PlainInternalFunctionReference

case class L3_PlainInternalFunctionReference(var name : String, var returnType : L3_Datatype) extends L3_InternalFunctionReference with L3_PlainFunctionReference {
  override def progress = ProgressLocation(L4_PlainInternalFunctionReference(name, returnType.progress))
}

/// L3_LeveledInternalFunctionReference

case class L3_LeveledInternalFunctionReference(var name : String, var level : Int, var returnType : L3_Datatype) extends L3_InternalFunctionReference with L3_LeveledFunctionReference {
  override def progress = ProgressLocation(L4_LeveledInternalFunctionReference(name, level, returnType.progress))
}
