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

package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.grid.l3.L3_Localization

object L3_DefaultProlongation {
  type EntryList = ListBuffer[(L3_ConstIndex, L3_Expression)]

  def generate(name : String, level : Int, numDims : Int, localization : L3_Localization, interpolation : String) : L3_Stencil = {
    val restriction = L3_DefaultRestriction.generate("dummy", level, numDims, localization, interpolation)

    var prolongation = L3_StencilOps.transpose(restriction)

    // apply scaling factors
    interpolation match {
      case "linear"          => prolongation = L3_StencilOps.scale(prolongation, math.pow(2, numDims))
      case "integral_linear" => // nothing to do
    }

    prolongation.name = name
    prolongation.level = level

    prolongation
  }
}
