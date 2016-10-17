package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.grid.GridEvaluator
import exastencils.logger._

/// IR_ResolveIntegrateFunction

object IR_ResolveIntegrateFunction extends DefaultStrategy("Resolve integration functions") {
  val functions = ListBuffer(
    "integrateOverEastFace", "integrateOverWestFace", "integrateOverNorthFace", "integrateOverSouthFace", "integrateOverTopFace", "integrateOverBottomFace",
    "integrateOverXStaggeredEastFace", "integrateOverXStaggeredNorthFace", "integrateOverXStaggeredTopFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",
    "integrateOverYStaggeredEastFace", "integrateOverYStaggeredNorthFace", "integrateOverYStaggeredTopFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",
    "integrateOverZStaggeredEastFace", "integrateOverZStaggeredNorthFace", "integrateOverZStaggeredTopFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")

  this += new Transformation("Resolving functions", {
    case IR_FunctionCall(function, args) if functions.contains(function.name) => {
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function ${ function.name } without arguments")
        IR_NullExpression
      } else {
        if (args.length > 1) Logger.warn(s"Trying to use build-in function ${ function.name } with more than one arguments; additional arguments are discarded")
        GridEvaluator.getEvaluator.invokeIntegrateResolve(function.name, args(0))
      }
    }
  })
}
