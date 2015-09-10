package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._

abstract class Grid {
  def invokeAccessResolve(specialField : SpecialFieldAccess) : Expression
  def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess) : Expression
  def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression
}

// helper object/method to branch grid types
object Grid {
  def getGridObject : Grid = {
    //    val gridType = "AxisAlignedVariableWidth" // TODO: move to knowledge
    val gridType = "AxisAlignedConstWidth" // TODO: move to knowledge
    gridType match {
      case "AxisAlignedConstWidth"    => Grid_AxisAlignedConstWidth
      case "AxisAlignedVariableWidth" => Grid_AxisAlignedVariableWidth
    }
  }
}

object ResolveSpecialFields extends DefaultStrategy("ResolveSpecialFields") {
  this += new Transformation("SearchAndReplace", {
    case specialField : SpecialFieldAccess => Grid.getGridObject.invokeAccessResolve(specialField)
  })
}

object ResolveGeometryFunctions extends DefaultStrategy("ResolveGeometryFunctions") {
  val evalFunctions = ListBuffer(
    "evalAtEastFace", "evalAtWestFace", "evalAtNorthFace", "evalAtSouthFace", "evalAtTopFace", "evalAtBottomFace")
  val integrateFunctions = ListBuffer(
    "integrateOverEastFace", "integrateOverWestFace", "integrateOverNorthFace", "integrateOverSouthFace", "integrateOverTopFace", "integrateOverBottomFace",
    "integrateOverXStaggeredEastFace", "integrateOverXStaggeredNorthFace", "integrateOverXStaggeredTopFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",
    "integrateOverYStaggeredEastFace", "integrateOverYStaggeredNorthFace", "integrateOverYStaggeredTopFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",
    "integrateOverZStaggeredEastFace", "integrateOverZStaggeredNorthFace", "integrateOverZStaggeredTopFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")

  this += new Transformation("SearchAndReplace", {
    case FunctionCallExpression(functionName, args) if evalFunctions.contains(functionName) => {
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function $functionName without arguments")
        NullExpression
      } else {
        if (args.length > 1) Logger.warn(s"Trying to use build-in function $functionName with more than one arguments; additional arguments are discarded")
        args(0) match {
          case access : FieldAccess => Grid.getGridObject.invokeEvalResolve(functionName, access)
          case _ => {
            Logger.warn(s"Argument ${args(0).prettyprint} is currently not supported for function $functionName")
            args(0)
          }
        }
      }
    }

    case FunctionCallExpression(functionName, args) if integrateFunctions.contains(functionName) => {
      Grid.getGridObject.invokeIntegrateResolve(functionName, args(0).asInstanceOf[Expression])
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

