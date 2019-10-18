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
import exastencils.base.l2._
import exastencils.boundary.l3._
import exastencils.prettyprinting._

/// L2_BoundaryCondition

trait L2_BoundaryCondition extends L2_Node with L2_Progressable with PrettyPrintable {
  override def progress : L3_BoundaryCondition
}

/// L2_NoBC

case object L2_NoBC extends L2_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "None"
  override def progress = ProgressLocation(L3_NoBC)
}
