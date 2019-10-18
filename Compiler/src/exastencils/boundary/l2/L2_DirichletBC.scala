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

package exastencils.boundary.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_Expression
import exastencils.boundary.l3.L3_DirichletBC
import exastencils.prettyprinting.PpStream

/// L2_DirichletBC

case class L2_DirichletBC(boundaryValue : L2_Expression) extends L2_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryValue
  override def progress = ProgressLocation(L3_DirichletBC(boundaryValue.progress))
}
