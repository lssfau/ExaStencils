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

package exastencils.util.l3

import exastencils.base.l3.L3_ConstIndex
import exastencils.config.Knowledge

object L3_OffsetAlias {

  def toConstIndex(dir : String, numDims : Int = /* FIXME */ Knowledge.dimensionality) = {
    val offset = L3_ConstIndex(Array.fill(numDims)(0))

    dir match {
      case "center" =>
      case "east"   => offset(0) = 1
      case "west"   => offset(0) = -1
      case "north"  => offset(1) = 1
      case "south"  => offset(1) = -1
      case "top"    => offset(2) = 1
      case "bottom" => offset(2) = -1
    }

    offset
  }
}
