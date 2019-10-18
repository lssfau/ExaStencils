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

import exastencils.polyhedron.IR_PolyScalarAccessLike
import exastencils.prettyprinting._

/// IR_VariableAccess

object IR_VariableAccess {
  def apply(decl : IR_VariableDeclaration) = new IR_VariableAccess(decl.name, decl.datatype)
}

case class IR_VariableAccess(var name : String, var datatype : IR_Datatype) extends IR_Access with IR_PolyScalarAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def uniqueID : String = name
}
