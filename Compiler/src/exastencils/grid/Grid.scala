package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._

abstract class Grid {
  def initL4()
  def generateInitCode() : ListBuffer[Statement]

  def applyStrategies() = {
    ResolveEvaluationFunctions.apply()
    ResolveIntegrationFunctions.apply()
    ResolveVirtualFields.apply()
  }

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

  // helper functions of offsetting indices and accesses
  def offsetIndex(index : MultiIndex, offset : Expression, dim : Int) : MultiIndex = {
    var modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : FieldAccess, offset : Expression, dim : Int) : FieldAccess = {
    var modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  // basic functionality
  def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression
  def cellCenter(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression

  def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression
  def gridWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = cellWidth(level, index, arrayIndex, dim) // simple alias for most grids
}

// helper object/method to branch grid types
object Grid {
  def getGridObject : Grid = {
    Knowledge.discr_gridType match {
      case "AxisAlignedConstWidth"    => Grid_AxisAlignedConstWidth
      case "AxisAlignedVariableWidth" => Grid_AxisAlignedVariableWidth
    }
  }
}
