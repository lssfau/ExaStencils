package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.logger._

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
    case FunctionCallExpression(functionName, args) if functions.contains(functionName) => {
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function $functionName without arguments")
        NullExpression
      } else {
        if (args.length > 2) Logger.warn(s"Trying to use build-in function $functionName with more than one arguments; additional arguments are discarded")
        args match {
          case ListBuffer(access : FieldAccess)                                 => Grid.getGridObject.invokeEvalResolve(functionName, access, "default")
          case ListBuffer(access : FieldAccess, interpolation : StringConstant) => Grid.getGridObject.invokeEvalResolve(functionName, access, interpolation.value)
          case _ => {
            Logger.warn(s"Arguments (${args.map(_.prettyprint).mkString(", ")}) are currently not supported for function $functionName")
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
    case FunctionCallExpression(functionName, args) if functions.contains(functionName) => {
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function $functionName without arguments")
        NullExpression
      } else {
        if (args.length > 1) Logger.warn(s"Trying to use build-in function $functionName with more than one arguments; additional arguments are discarded")
        Grid.getGridObject.invokeIntegrateResolve(functionName, args(0))
      }
    }
  })
}

object ResolveVirtualFields extends DefaultStrategy("ResolveVirtualFields") {
  this += new Transformation("SearchAndReplace", {
    case virtualField : VirtualFieldAccess => Grid.getGridObject.invokeAccessResolve(virtualField)
  })
}

object CollectFieldAccesses extends QuietDefaultStrategy("Collecting field accesses") {
  var fieldAccesses : ListBuffer[FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[VirtualFieldAccess] = ListBuffer()

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
    case fieldAccess : FieldAccess =>
      fieldAccesses += fieldAccess
      fieldAccess
    case fieldAccess : VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}

object ShiftFieldAccessIndices extends QuietDefaultStrategy("Shifting indices of field accesses") {
  var offset : Expression = 0
  var dim : Int = 0

  this += new Transformation("Searching and shifting", {
    case fieldAccess : FieldAccess =>
      fieldAccess.index(dim) += offset
      fieldAccess
    case fieldAccess : VirtualFieldAccess =>
      fieldAccess.index(dim) += offset
      fieldAccess
  })
}

object ReplaceFieldAccesses extends QuietDefaultStrategy("Replace field accesses with another expression") {
  var replacement : Expression = NullExpression

  this += new Transformation("SearchAndReplace", {
    case _ : FieldAccess => Duplicate(replacement)
  }, false)
}
