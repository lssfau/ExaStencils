package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.logger._

object PrepareShiftedEvaluationFunctions extends DefaultStrategy("PrepareShiftedEvaluationFunctions") {
  // TODO: this strategy is currently necessary for cases where evalLeft functions are wrapped in integrateLeft functions
  //       these would be shifted twice, hence this workaround removing one of these shifts by replacing the eval

  val shiftIntegrateFunctions = ListBuffer(
    "integrateOverWestFace", "integrateOverSouthFace", "integrateOverBottomFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")
  val shiftEvalFunctions = Map[String, String](
    ("evalAtWestFace" -> "evalAtEastFace"),
    ("evalAtSouthFace" -> "evalAtNorthFace"),
    ("evalAtBottomFace" -> "evalAtTopFace"),
    ("evalAtXStaggeredSouthFace" -> "evalAtXStaggeredNorthFace"),
    ("evalAtXStaggeredBottomFace" -> "evalAtXStaggeredTopFace"),
    ("evalAtYStaggeredWestFace" -> "evalAtYStaggeredEastFace"),
    ("evalAtYStaggeredSouthFace" -> "evalAtYStaggeredNorthFace"),
    ("evalAtYStaggeredBottomFace" -> "evalAtYStaggeredTopFace"),
    ("evalAtZStaggeredWestFace" -> "evalAtZStaggeredEastFace"),
    ("evalAtZStaggeredSouthFace" -> "evalAtZStaggeredNorthFace"),
    ("evalAtZStaggeredBottomFace" -> "evalAtZStaggeredTopFace"))

  private object DoShift extends QuietDefaultStrategy("DoShift") {
    this += new Transformation("Resolving functions", {
      case fct @ IR_FunctionCall(function, args) if shiftEvalFunctions.contains(function.name) => {
        // return datatype remains identical
        fct.function.name = shiftEvalFunctions(function.name)
        fct
      }
    })
  }

  this += new Transformation("Resolving functions", {
    case fct @ IR_FunctionCall(function, args) if shiftIntegrateFunctions.contains(function.name) => {
      DoShift.applyStandalone(fct)
      fct
    }
  })
}

object ResolveEvaluationFunctions extends DefaultStrategy("ResolveEvaluationFunctions") {
  val functions = ListBuffer(
    "evalAtEastFace", "evalAtWestFace", "evalAtNorthFace", "evalAtSouthFace", "evalAtTopFace", "evalAtBottomFace",
    "evalAtXStaggeredEastFace", "evalAtXStaggeredNorthFace", "evalAtXStaggeredTopFace",
    "evalAtXStaggeredWestFace", "evalAtXStaggeredSouthFace", "evalAtXStaggeredBottomFace",
    "evalAtYStaggeredEastFace", "evalAtYStaggeredNorthFace", "evalAtYStaggeredTopFace",
    "evalAtYStaggeredWestFace", "evalAtYStaggeredSouthFace", "evalAtYStaggeredBottomFace",
    "evalAtZStaggeredEastFace", "evalAtZStaggeredNorthFace", "evalAtZStaggeredTopFace",
    "evalAtZStaggeredWestFace", "evalAtZStaggeredSouthFace", "evalAtZStaggeredBottomFace")

  this += new Transformation("Resolving functions", {
    case IR_FunctionCall(function, args) if functions.contains(function.name) => {
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function ${ function.name } without arguments")
        IR_NullExpression
      } else {
        if (args.length > 2) Logger.warn(s"Trying to use build-in function ${ function.name } with more than one arguments; additional arguments are discarded")
        args match {
          case ListBuffer(access : IR_FieldAccess)                                    => GridEvaluator.getEvaluator.invokeEvalResolve(function.name, access, "default")
          case ListBuffer(access : IR_FieldAccess, interpolation : IR_StringConstant) => GridEvaluator.getEvaluator.invokeEvalResolve(function.name, access, interpolation.value)
          case _                                                                      => {
            Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function ${ function.name }")
            args(0)
          }
        }
      }
    }
  })
}

object ResolveIntegrationFunctions extends DefaultStrategy("ResolveIntegrateFunctions") {
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

object ExpandEvaluationFunctions extends DefaultStrategy("ExpandEvaluationFunctions") {
  this += new Transformation("Expanding evaluation functions", {
    case eval : GridEvaluator_AxisAligned.EvalAtRFace => eval.expandSpecial
  })
}

object ResolveVirtualFields extends DefaultStrategy("ResolveVirtualFields") {
  this += new Transformation("SearchAndReplace", {
    case virtualField : IR_VirtualFieldAccess => GridGeometry.getGeometry.invokeAccessResolve(virtualField)
  })
}

object CollectFieldAccesses extends QuietDefaultStrategy("Collecting field accesses") {
  var fieldAccesses : ListBuffer[IR_FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[IR_VirtualFieldAccess] = ListBuffer()

  override def apply(node : Option[Node] = None) = {
    fieldAccesses.clear
    vFieldAccesses.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    fieldAccesses.clear
    vFieldAccesses.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collecting", {
    case fieldAccess : IR_FieldAccess        =>
      fieldAccesses += fieldAccess
      fieldAccess
    case fieldAccess : IR_VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}

object ShiftFieldAccessIndices extends QuietDefaultStrategy("Shifting indices of field accesses") {
  var offset : IR_Expression = 0
  var dim : Int = 0

  this += new Transformation("Searching and shifting", {
    case fieldAccess : IR_FieldAccess        =>
      fieldAccess.index(dim) += offset
      fieldAccess
    case fieldAccess : IR_VirtualFieldAccess =>
      fieldAccess.index(dim) += offset
      fieldAccess
  })
}

object ReplaceFieldAccesses extends QuietDefaultStrategy("Replace field accesses with another expression") {
  var replacement : IR_Expression = IR_NullExpression

  this += new Transformation("SearchAndReplace", {
    case _ : IR_FieldAccess => Duplicate(replacement)
  }, false)
}
