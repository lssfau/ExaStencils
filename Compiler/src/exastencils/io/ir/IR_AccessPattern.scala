package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Node
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.config.Knowledge
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_Localization

object IR_AccessPattern {
  def apply(callback : IR_Index => IR_Access) : IR_AccessPattern = new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = None)
  def apply(callback : IR_Index => IR_Access, accessIndices : Option[ListBuffer[IR_Index]]) : IR_AccessPattern =
    new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices)
}

/// IR_AccessPattern
// consists of callback function and members to describe different access patterns
// callbacks are registered and called by IR_DataBuffer (e.g. IR_FieldAccess for underlying IR_Field)
// provides helper functions to deal with non-regular access patterns

case class IR_AccessPattern(
    var accessCallbackFuntion : IR_Index => IR_Access, // callback function to be registered and used by IR_DataBuffer wrapper
    var accessIndices : Option[ListBuffer[IR_Index]], // contains N indices to be accessed for each grid element (e.g. nodes/cells/...)
) extends IR_Node {

  // specifies if elements are accessed regularly
  def isRegular : Boolean = !isAccessPatternSWE

  def numAccesses = if (accessIndices.isDefined) accessIndices.get.length else 1

  // special access pattern for nodal fields in SWE (e.g. node positions and bath) applications
  // 6 elements are accesses per grid cell
  def isAccessPatternSWE : Boolean = accessIndices.isDefined

  def callAccessFunction(index : IR_Index) : IR_Access = accessCallbackFuntion(index)

  // get list of accesses for each index defined by the pattern
  def accessesForPattern(indices : IR_Index*) : ListBuffer[ListBuffer[IR_Access]] = if (isAccessPatternSWE) {
    accessIndices.get.map(accIdx => indices.map(idx => callAccessFunction(accIdx + idx)).to[ListBuffer])
  } else {
    ListBuffer(indices.map(callAccessFunction).to[ListBuffer])
  }

  // transform index range depending on access pattern
  def transformExpressionIndexRange(start : IR_ExpressionIndex, end : IR_ExpressionIndex) : IR_ExpressionIndexRange = {
    if (isAccessPatternSWE) {
      IR_ExpressionIndexRange(start, IR_ExpressionIndex(end.indices.map(_ - 1) : _*))
    } else {
      IR_ExpressionIndexRange(start, end)
    }
  }

  // transform dimensionality depending on access pattern
  def transformDataExtents(dims : ListBuffer[IR_Expression], localization : IR_Localization, orderKJI : Boolean) : ListBuffer[IR_Expression] = {
    if (isAccessPatternSWE && localization == IR_AtNode) {
      // 6 accesses per grid cell instead of 1 access per grid node
      val numDims = Knowledge.dimensionality
      val nodalToZonal = (if (orderKJI) dims.reverse else dims).zipWithIndex.map { case (dim, idx) => if (idx < numDims) dim-1 else dim }
      val newDims = IR_IntegerConstant(numAccesses) +: nodalToZonal
      if (orderKJI) newDims.reverse else newDims
    } else {
      dims
    }
  }
}

