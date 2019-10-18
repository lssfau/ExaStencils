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

package exastencils.base.ir

/// IR_InternalFunctionReference

object IR_InternalFunctionReference {
  def floord = IR_PlainInternalFunctionReference("floord", IR_IntegerDatatype)
}

trait IR_InternalFunctionReference extends IR_FunctionReference

/// IR_PlainInternalFunctionReference

case class IR_PlainInternalFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_InternalFunctionReference with IR_PlainFunctionReference {
}

/// IR_LeveledInternalFunctionReference

case class IR_LeveledInternalFunctionReference(var baseName : String, var level : Int, var returnType : IR_Datatype) extends IR_InternalFunctionReference with IR_LeveledFunctionReference {
  override var name = baseName + '_' + level
}
