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

package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._

/// L1_InternalFunctionReference

object L1_InternalFunctionReference {
  def floord = L1_PlainInternalFunctionReference("floord", L1_RealDatatype/* TODO: check data type */)
}

trait L1_InternalFunctionReference extends L1_FunctionReference

/// L1_PlainInternalFunctionReference

case class L1_PlainInternalFunctionReference(var name : String, var returnType : L1_Datatype) extends L1_InternalFunctionReference with L1_PlainFunctionReference {
  override def progress = ProgressLocation(L2_PlainInternalFunctionReference(name, returnType.progress))
}

/// L1_LeveledInternalFunctionReference

case class L1_LeveledInternalFunctionReference(var name : String, var level : Int, var returnType : L1_Datatype) extends L1_InternalFunctionReference with L1_LeveledFunctionReference {
  override def progress = ProgressLocation(L2_LeveledInternalFunctionReference(name, level, returnType.progress))
}
