package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_Localization
import exastencils.logger.Logger

abstract class IR_AccessFunction extends IR_Node {
  def get(idx : IR_Index) : IR_Access
}

case class IR_AccessFieldFunction(var field : IR_Field, var slot : IR_Expression, var hodtComponentIndex : Option[IR_Index] = None) extends IR_AccessFunction {
  override def get(idx : IR_Index) : IR_Access = {
    val index = if (hodtComponentIndex.isDefined)
      IR_ExpressionIndex(idx.toExpressionIndex.indices ++ hodtComponentIndex.get.toExpressionIndex.indices)
    else
      idx
    IR_FieldAccess(field, slot, index.toExpressionIndex)
  }
}

case class IR_AccessTempBufferFunction(var tempBuffer : IR_IV_TemporaryBuffer) extends IR_AccessFunction {
  override def get(idx : IR_Index) = IR_IV_TemporaryBuffer.accessArray(tempBuffer, idx)
}

/// IR_AccessPattern
// consists of callback function and members to describe different access patterns
// callbacks are registered and called by IR_DataBuffer (e.g. IR_FieldAccess for underlying IR_Field)
// provides helper functions to deal with non-regular access patterns

abstract class IR_AccessPattern extends IR_Node {

  def accessFunction : IR_AccessFunction // callback function to be registered and used by IR_DataBuffer wrapper
  def accessIndices : Option[ListBuffer[IR_Index]] // contains N indices to be accessed for each grid element (e.g. nodes/cells/...)

  def numAccesses : Int = if (accessIndices.isDefined) accessIndices.get.length else 1

  def callAccessFunction(index : IR_Index) : IR_Access = accessFunction.get(index)

  // get list of accesses for each index defined by the pattern
  def accessesForPattern(indices : IR_Index*) : ListBuffer[ListBuffer[IR_Access]]

  // transform index range depending on access pattern
  def transformExpressionIndexRange(start : IR_ExpressionIndex, end : IR_ExpressionIndex) : IR_ExpressionIndexRange

  // transform dimensionality depending on access pattern
  def transformDataExtents(dims : ListBuffer[IR_Expression], localization : IR_Localization, orderKJI : Boolean) : ListBuffer[IR_Expression]
}

// regular access pattern. no special treatment required
case class IR_RegularAccessPattern(var accessFunction : IR_AccessFunction) extends IR_AccessPattern {

  override def accessIndices : Option[ListBuffer[IR_Index]] = None

  override def accessesForPattern(indices : IR_Index*) : ListBuffer[ListBuffer[IR_Access]] =
    ListBuffer(indices.map(callAccessFunction).to[ListBuffer])

  def transformExpressionIndexRange(start : IR_ExpressionIndex, end : IR_ExpressionIndex) : IR_ExpressionIndexRange =
    IR_ExpressionIndexRange(start, end)

  def transformDataExtents(dims : ListBuffer[IR_Expression], localization : IR_Localization, orderKJI : Boolean) : ListBuffer[IR_Expression] =
    dims
}

// special access pattern for nodal fields in SWE (e.g. node positions and bath) applications
// 6 elements are accesses per grid cell
case class IR_SWEAccessPattern(
    var accessFunction : IR_AccessFunction,
    var accessIndices : Option[ListBuffer[IR_Index]]) extends IR_AccessPattern {

  if (accessIndices.isEmpty || (accessIndices.isDefined && accessIndices.get.size != 6))
    Logger.error("Wrong input args for \"IR_SWEAccessPattern\"")

  override def accessesForPattern(indices : IR_Index*) : ListBuffer[ListBuffer[IR_Access]] =
    accessIndices.get.map(accIdx => indices.map(idx => callAccessFunction(accIdx + idx)).to[ListBuffer])

  def transformExpressionIndexRange(start : IR_ExpressionIndex, end : IR_ExpressionIndex) : IR_ExpressionIndexRange =
    IR_ExpressionIndexRange(start, IR_ExpressionIndex(end.indices.map(_ - 1) : _*))

  def transformDataExtents(dims : ListBuffer[IR_Expression], localization : IR_Localization, orderKJI : Boolean) : ListBuffer[IR_Expression] = {
    // 6 accesses per grid cell instead of 1 access per grid node
    val numDims = Knowledge.dimensionality
    val nodalToZonal = (if (orderKJI) dims.reverse else dims).zipWithIndex.map { case (dim, idx) => if (idx < numDims) dim - 1 else dim }
    val newDims = IR_IntegerConstant(numAccesses) +: nodalToZonal
    if (orderKJI) newDims.reverse else newDims
  }
}