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
  def initL4()
  def generateInitCode() : ListBuffer[Statement]

  def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method]

  // helper method to map names of special fields to actual member functions implementing the resolving step
  def invokeAccessResolve(virtualField : VirtualFieldAccess) : Expression = {
    var functionName = virtualField.fieldName
    if (functionName.startsWith("vf_")) functionName = functionName.substring(3)
    functionName.substring(functionName.length() - 2) match {
      case "_x" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 0 : Integer).asInstanceOf[Expression]
      }
      case "_y" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 1 : Integer).asInstanceOf[Expression]
      }
      case "_z" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 2 : Integer).asInstanceOf[Expression]
      }
      case _ => {
        val method = resolveGridMemberFunction(functionName)
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex).asInstanceOf[Expression]
      }
    }
  }

  def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess, interpolation : String) : Expression
  def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression
}

// helper object/method to branch grid types
object Grid {
  def getGridObject : Grid = {
    //    val gridType = "AxisAlignedVariableWidth" // TODO: move to knowledge
    val gridType = "AxisAlignedConstWidth" // TODO: move to knowledge
    Knowledge.discr_gridType match {
      case "AxisAlignedConstWidth"    => Grid_AxisAlignedConstWidth
      case "AxisAlignedVariableWidth" => Grid_AxisAlignedVariableWidth
    }
  }
}

object ResolveVirtualFields extends DefaultStrategy("ResolveVirtualFields") {
  this += new Transformation("SearchAndReplace", {
    case virtualField : VirtualFieldAccess => Grid.getGridObject.invokeAccessResolve(virtualField)
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

    case FunctionCallExpression(functionName, args) if integrateFunctions.contains(functionName) => {
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
