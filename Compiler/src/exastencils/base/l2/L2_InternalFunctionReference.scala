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

package exastencils.base.l2

import exastencils.base.ProgressLocation
import exastencils.base.l3._

/// L2_InternalFunctionReference

object L2_InternalFunctionReference {
  def floord = L2_PlainInternalFunctionReference("floord", L2_RealDatatype/* TODO: check data type */)
}

trait L2_InternalFunctionReference extends L2_FunctionReference

/// L2_PlainInternalFunctionReference

case class L2_PlainInternalFunctionReference(var name : String, var returnType : L2_Datatype) extends L2_InternalFunctionReference with L2_PlainFunctionReference {
  override def progress = ProgressLocation(L3_PlainInternalFunctionReference(name, returnType.progress))
}

/// L2_LeveledInternalFunctionReference

case class L2_LeveledInternalFunctionReference(var name : String, var level : Int, var returnType : L2_Datatype) extends L2_InternalFunctionReference with L2_LeveledFunctionReference {
  override def progress = ProgressLocation(L3_LeveledInternalFunctionReference(name, level, returnType.progress))
}
