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

package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.grid.l4.L4_Localization

object L4_DefaultProlongation {
  type EntryList = ListBuffer[(L4_ConstIndex, L4_Expression)]

  def generate(name : String, level : Int, numDims : Int, localization : L4_Localization, interpolation : String) : L4_Stencil = {
    val restriction = L4_DefaultRestriction.generate("dummy", level, numDims, localization, interpolation)

    var prolongation = L4_StencilOps.transpose(restriction)

    // apply scaling factors
    interpolation match {
      case "linear"          => prolongation = L4_StencilOps.scale(prolongation, math.pow(2, numDims))
      case "integral_linear" => // nothing to do
    }

    prolongation.name = name
    prolongation.level = level

    prolongation
  }
}
