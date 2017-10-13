package exastencils.operator.l3

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.optimization.l3._
import exastencils.prettyprinting._
import exastencils.util.l3._

/// L3_StencilEntry

abstract class L3_StencilEntry extends L3_Node with L3_Progressable with PrettyPrintable {
  override def progress : L4_StencilEntry

  def asStencilOffsetEntry : L3_StencilOffsetEntry
  def asStencilMappingEntry : L3_StencilMappingEntry

  def numDims : Int
  def colStride : Array[Double]

  var coefficient : L3_Expression
}

/// L3_StencilOffsetEntry

case class L3_StencilOffsetEntry(var offset : L3_ConstIndex, var coefficient : L3_Expression) extends L3_StencilEntry {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  override def progress = L4_StencilOffsetEntry(offset.progress, coefficient.progress)

  override def asStencilOffsetEntry = this
  override def asStencilMappingEntry = {
    def defIt = L3_ExpressionIndex((0 until numDims).toArray.map(L3_FieldIteratorAccess(_) : L3_Expression))
    L3_StencilMappingEntry(defIt, defIt + offset, coefficient)
  }

  override def numDims = offset.length
  override def colStride = Array.fill(numDims)(1.0)
}

/// L3_StencilOffsetEntry

case class L3_StencilMappingEntry(var row : L3_ExpressionIndex, var col : L3_ExpressionIndex, var coefficient : L3_Expression) extends L3_StencilEntry {

  L3_ReplaceIntWithReal.applyStandalone(row)
  L3_ReplaceIntWithReal.applyStandalone(col)

  override def prettyprint(out : PpStream) = {
    L3_GeneralSimplify.doUntilDoneStandalone(this)
    out << row << " from " << col << " with " << coefficient
  }

  override def progress = L4_StencilMappingEntry(row.progress, col.progress, coefficient.progress)

  override def asStencilOffsetEntry = {
    val offset = Duplicate(col)

    for (d <- 0 until row.length) {
      L3_ReplaceExpressions.toReplace = row.indices(d)
      L3_ReplaceExpressions.replacement = 0
      L3_ReplaceExpressions.applyStandalone(offset)
    }

    offset.indices.transform(L3_SimplifyExpression.simplifyFloatingExpr)

    L3_ReplaceRealWithInt.applyStandalone(offset)
    L3_GeneralSimplify.doUntilDoneStandalone(offset)

    L3_StencilOffsetEntry(offset.toConstIndex, coefficient)
  }

  override def asStencilMappingEntry = this

  override def numDims = {
    if (row.length != col.length)
      Logger.warn(s"Size mismatch: ${ row.length } != ${ col.length }")
    row.length
  }

  override def colStride = {
    val stride = Duplicate(col)
    val sthLarge = 2 * 2 * 2 * 3 * 3 * 3 * 5 * 5 * 7 * 7

    for (d <- 0 until row.length) {
      L3_ReplaceExpressions.toReplace = row.indices(d)
      L3_ReplaceExpressions.replacement = sthLarge
      L3_ReplaceExpressions.applyStandalone(stride)
    }

    stride.indices.transform(L3_SimplifyExpression.simplifyFloatingExpr)
    stride.indices.map { v =>
      val stride = v.asInstanceOf[L3_RealConstant].value / sthLarge
      if (stride > 1) stride.round.toDouble
      else 1.0 / (1.0 / stride).round.toDouble
    }
  }

  def transpose() = {
    val tmp = row
    row = col
    col = tmp
  }
}
