package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.grid.ir._
import exastencils.logger._

/// GridGeometry

object GridGeometry {
  def getGeometry = {
    if (Knowledge.grid_isUniform && !Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_uniform_nonStaggered_AA
    else if (Knowledge.grid_isUniform && Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_uniform_staggered_AA
    else if (!Knowledge.grid_isUniform && !Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_nonUniform_nonStaggered_AA
    else if (!Knowledge.grid_isUniform && Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_nonUniform_staggered_AA
    else if (!Knowledge.grid_isUniform && !Knowledge.grid_isStaggered && !Knowledge.grid_isAxisAligned)
      GridGeometry_nonAA
    else
      Logger.error(s"Trying to get geometry for unsupported configuration of ( uniform : ${ Knowledge.grid_isUniform } ), ( staggered : ${ Knowledge.grid_isStaggered } ), ( axis-aligned : ${ Knowledge.grid_isAxisAligned } )")
  }
}

abstract class GridGeometry() {
  // information always required
  def nodePosAsVec(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = ???
  def nodePosition(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression

  def cellCenAsVec(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = ???
  def cellCenter(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression

  def cellWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression
  def gridWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = cellWidth(level, index, arrayIndex, dim) // simple alias for most grids

  def cellVolume(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = {
    var exp : IR_Expression = cellWidth(level, index, arrayIndex, 0)
    for (dim <- 1 until Knowledge.dimensionality)
      exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

  def cellCenterToFace(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = { 0.5 * cellWidth(level, index, arrayIndex, dim) }

  // resolution of special function accessing virtual fields
  def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass.getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  // helper method to map names of special fields to actual member functions implementing the resolving step
  def invokeAccessResolve(virtualField : IR_VirtualFieldAccess) : IR_Expression = {
    var functionName = virtualField.target.name
    if (functionName.startsWith("vf_")) functionName = functionName.substring(3)
    functionName.substring(functionName.length() - 2) match {
      case "_x" =>
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level : Integer, virtualField.index, None, 0 : Integer).asInstanceOf[IR_Expression]

      case "_y" =>
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level : Integer, virtualField.index, None, 1 : Integer).asInstanceOf[IR_Expression]

      case "_z" =>
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level : Integer, virtualField.index, None, 2 : Integer).asInstanceOf[IR_Expression]

      case _ =>
        val method = resolveGridMemberFunction(functionName)
        if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level : Integer, virtualField.index, None).asInstanceOf[IR_Expression]

    }
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  def initL4() : Unit
  def generateInitCode() : ListBuffer[IR_Statement]
}
