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

package exastencils.boundary.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.boundary.l4._
import exastencils.prettyprinting._

/// L3_BoundaryCondition

trait L3_BoundaryCondition extends L3_Node with L3_Progressable with PrettyPrintable {
  override def progress : L4_BoundaryCondition
}

/// L3_NoBC

case object L3_NoBC extends L3_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "None"
  override def progress = ProgressLocation(L4_NoBC)
}
