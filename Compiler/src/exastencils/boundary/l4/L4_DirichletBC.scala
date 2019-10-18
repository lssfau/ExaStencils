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
import exastencils.base.l4.L4_Expression
import exastencils.boundary.ir.IR_DirichletBC
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// L4_DirichletBC

object L4_DirichletBC {
  def apply(boundaryValue : L4_Expression) = new L4_DirichletBC(boundaryValue, Knowledge.discr_defaultDirichletOrder)
}

case class L4_DirichletBC(var boundaryValue : L4_Expression, var order : Int) extends L4_BoundaryCondition {
  override def prettyprint(out : PpStream) = out << boundaryValue
  override def progress = ProgressLocation(IR_DirichletBC(boundaryValue.progress, order))
}
