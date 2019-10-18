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

package exastencils.util.l1

import exastencils.base.l1._
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._

object L1_ResolveSpecialConstants extends DefaultStrategy("Resolve special constants") {
  this += new Transformation("special functions and constants", {
    // math constants
    case access : L1_UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
      L1_RealConstant(math.Pi)

    case access : L1_UnresolvedAccess if "SQRT2" == access.name || "M_SQRT2" == access.name =>
      L1_RealConstant(math.sqrt(2.0))
  })
}
