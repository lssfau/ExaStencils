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

package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l2.L2_LevelCollector

/// L2_IntegrateFunctions

object L2_IntegrateFunctions {
  val functions = HashSet[String](
    "integrateOverEastFace", "integrateOverWestFace",
    "integrateOverNorthFace", "integrateOverSouthFace",
    "integrateOverTopFace", "integrateOverBottomFace",

    "integrateOverXStaggeredEastFace", "integrateOverXStaggeredNorthFace", "integrateOverXStaggeredTopFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",

    "integrateOverYStaggeredEastFace", "integrateOverYStaggeredNorthFace", "integrateOverYStaggeredTopFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",

    "integrateOverZStaggeredEastFace", "integrateOverZStaggeredNorthFace", "integrateOverZStaggeredTopFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")

  def exists(fctName : String) = functions.contains(fctName)
}

/// L2_ResolveIntegrateFunctions

object L2_ResolveIntegrateFunctions extends DefaultStrategy("Resolve grid function references (integrate)") {
  val collector = new L2_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case L2_FunctionCall(ref : L2_UnresolvedFunctionReference, args) if L2_IntegrateFunctions.exists(ref.name) =>
      val level = {
        if (ref.level.isDefined) ref.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for reference to ${ ref.name }")
      }

      L2_IntegrateOnGrid(ref.name, level, args, ref.offset)
  })
}
