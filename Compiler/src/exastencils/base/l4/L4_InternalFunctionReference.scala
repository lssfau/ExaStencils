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

package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._

/// L4_InternalFunctionReference

object L4_InternalFunctionReference {
  def floord = L4_PlainInternalFunctionReference("floord", L4_RealDatatype/* TODO: check data type */)
}

trait L4_InternalFunctionReference extends L4_FunctionReference

/// L4_PlainInternalFunctionReference

case class L4_PlainInternalFunctionReference(var name : String, var returnType : L4_Datatype) extends L4_InternalFunctionReference with L4_PlainFunctionReference {
  override def progress = ProgressLocation(IR_PlainInternalFunctionReference(name, returnType.progress))
}

/// L4_LeveledInternalFunctionReference

case class L4_LeveledInternalFunctionReference(var name : String, var level : Int, var returnType : L4_Datatype) extends L4_InternalFunctionReference with L4_LeveledFunctionReference {
  override def progress = ProgressLocation(IR_LeveledInternalFunctionReference(name, level, returnType.progress))
}
