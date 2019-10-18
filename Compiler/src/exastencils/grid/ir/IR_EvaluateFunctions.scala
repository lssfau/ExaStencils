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

package exastencils.grid.ir

import scala.collection.mutable._

import exastencils.datastructures._

/// IR_PrepareShiftedEvaluationFunctions

object IR_PrepareShiftedEvaluationFunctions extends DefaultStrategy("Prepare shifted evaluation functions") {
  // TODO: this strategy is currently necessary for cases where evalLeft functions are wrapped in integrateLeft functions
  //       these would be shifted twice, hence this workaround removing one of these shifts by replacing the eval

  val shiftIntegrateFunctions = ListBuffer(
    "integrateOverWestFace", "integrateOverSouthFace", "integrateOverBottomFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")

  val shiftEvalFunctions = Map[String, String](
    "evalAtWestFace" -> "evalAtEastFace",
    "evalAtSouthFace" -> "evalAtNorthFace",
    "evalAtBottomFace" -> "evalAtTopFace",
    "evalAtXStaggeredSouthFace" -> "evalAtXStaggeredNorthFace",
    "evalAtXStaggeredBottomFace" -> "evalAtXStaggeredTopFace",
    "evalAtYStaggeredWestFace" -> "evalAtYStaggeredEastFace",
    "evalAtYStaggeredSouthFace" -> "evalAtYStaggeredNorthFace",
    "evalAtYStaggeredBottomFace" -> "evalAtYStaggeredTopFace",
    "evalAtZStaggeredWestFace" -> "evalAtZStaggeredEastFace",
    "evalAtZStaggeredSouthFace" -> "evalAtZStaggeredNorthFace",
    "evalAtZStaggeredBottomFace" -> "evalAtZStaggeredTopFace")

  private object DoShift extends QuietDefaultStrategy("Do shift") {
    this += new Transformation("Resolving functions", {
      case access : IR_EvaluateOnGrid if shiftEvalFunctions.contains(access.name) =>
        // datatype remains unchanged
        access.name = shiftEvalFunctions(access.name)
        access
    })
  }

  this += new Transformation("Resolving functions", {
    case access : IR_IntegrateOnGrid if shiftIntegrateFunctions.contains(access.name) =>
      DoShift.applyStandalone(access)
      access
  })
}
