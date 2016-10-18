package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid._
import exastencils.logger._

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

  private object DoShift extends QuietDefaultStrategy("DoShift") {
    this += new Transformation("Resolving functions", {
      case fct @ IR_FunctionCall(function, args) if shiftEvalFunctions.contains(function.name) =>
        // return datatype remains identical
        fct.function.name = shiftEvalFunctions(function.name)
        fct
    })
  }

  this += new Transformation("Resolving functions", {
    case fct @ IR_FunctionCall(function, args) if shiftIntegrateFunctions.contains(function.name) =>
      DoShift.applyStandalone(fct)
      fct
  })
}

/// IR_ResolveEvalFunction

object IR_ResolveEvalFunction extends DefaultStrategy("Resolve evaluation functions") {
  val functions = ListBuffer(
    "evalAtEastFace", "evalAtWestFace", "evalAtNorthFace", "evalAtSouthFace", "evalAtTopFace", "evalAtBottomFace",
    "evalAtXStaggeredEastFace", "evalAtXStaggeredNorthFace", "evalAtXStaggeredTopFace",
    "evalAtXStaggeredWestFace", "evalAtXStaggeredSouthFace", "evalAtXStaggeredBottomFace",
    "evalAtYStaggeredEastFace", "evalAtYStaggeredNorthFace", "evalAtYStaggeredTopFace",
    "evalAtYStaggeredWestFace", "evalAtYStaggeredSouthFace", "evalAtYStaggeredBottomFace",
    "evalAtZStaggeredEastFace", "evalAtZStaggeredNorthFace", "evalAtZStaggeredTopFace",
    "evalAtZStaggeredWestFace", "evalAtZStaggeredSouthFace", "evalAtZStaggeredBottomFace")

  this += new Transformation("Resolving functions", {
    case IR_FunctionCall(function, args) if functions.contains(function.name) =>
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function ${ function.name } without arguments")
        IR_NullExpression
      } else {
        if (args.length > 2) Logger.warn(s"Trying to use build-in function ${ function.name } with more than one arguments; additional arguments are discarded")
        args match {
          case ListBuffer(access : IR_FieldAccess)                                    => GridEvaluator.getEvaluator.invokeEvalResolve(function.name, access, "default")
          case ListBuffer(access : IR_FieldAccess, interpolation : IR_StringConstant) => GridEvaluator.getEvaluator.invokeEvalResolve(function.name, access, interpolation.value)
          case _                                                                      =>
            Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function ${ function.name }")
            args(0)
        }
      }
  })
}

/// IR_ExpandEvalFunction

object IR_ExpandEvalFunction extends DefaultStrategy("Expand evaluation functions") {
  this += new Transformation("Expanding evaluation functions", {
    case eval : GridEvaluator_AxisAligned.EvalAtRFace => eval.expandSpecial
  })
}
