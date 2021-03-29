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

import exastencils.prettyprinting.PpStream

/// IR_ExternalFunctionReference

object IR_ExternalFunctionReference {
  def printf = new IR_ExternalFunctionReference("printf", IR_UnitDatatype)
  def fabs = new IR_ExternalFunctionReference("fabs", IR_RealDatatype)

  def apply(name : String) = new IR_ExternalFunctionReference(name, IR_UnknownDatatype)
}

case class IR_ExternalFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}
