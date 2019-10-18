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

import exastencils.prettyprinting._

/// IR_FunctionReference

trait IR_FunctionReference extends IR_Node with PrettyPrintable {
  var name : String
  def returnType : IR_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// IR_PlainFunctionReference

trait IR_PlainFunctionReference extends IR_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}

/// IR_LeveledFunctionReference

trait IR_LeveledFunctionReference extends IR_FunctionReference {
  def baseName : String
  def level : Int
  override def prettyprint(out : PpStream) = out << name
}

/// IR_UnresolvedFunctionReference

case class IR_UnresolvedFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference
