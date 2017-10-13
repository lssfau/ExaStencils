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
