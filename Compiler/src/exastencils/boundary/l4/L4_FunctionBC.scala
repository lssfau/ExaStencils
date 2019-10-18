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

package exastencils.boundary.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.boundary.ir.IR_FunctionBC
import exastencils.prettyprinting.PpStream

/// L4_FunctionBC

// wraps a user-provided function implementing boundary handling
case class L4_FunctionBC(var boundaryFunction : L4_FunctionCall) extends L4_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryFunction
  override def progress = ProgressLocation(IR_FunctionBC(boundaryFunction.progress))
}
