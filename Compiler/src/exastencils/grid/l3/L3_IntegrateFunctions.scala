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

package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l3.L3_LevelCollector

/// L3_IntegrateFunctions

object L3_IntegrateFunctions {
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

/// L3_ResolveIntegrateFunctions

object L3_ResolveIntegrateFunctions extends DefaultStrategy("Resolve grid function references (integrate)") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case L3_FunctionCall(ref : L3_UnresolvedFunctionReference, args) if L3_IntegrateFunctions.exists(ref.name) =>
      val level = {
        if (ref.level.isDefined) ref.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for reference to ${ ref.name }")
      }

      L3_IntegrateOnGrid(ref.name, level, args, ref.offset)
  })
}
