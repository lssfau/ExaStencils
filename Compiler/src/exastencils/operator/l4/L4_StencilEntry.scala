package exastencils.operator.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.operator.ir._
import exastencils.optimization.l4._
import exastencils.prettyprinting._
import exastencils.util.l4._

/// L4_StencilEntry

abstract class L4_StencilEntry extends L4_Node with L4_Progressable with PrettyPrintable {
  override def progress : IR_StencilEntry

  def asStencilOffsetEntry : L4_StencilOffsetEntry
  def asStencilMappingEntry : L4_StencilMappingEntry

  def numDims : Int
  def colStride : Array[Double]

  var coefficient : L4_Expression
}

/// L4_StencilOffsetEntry

case class L4_StencilOffsetEntry(var offset : L4_ConstIndex, var coefficient : L4_Expression) extends L4_StencilEntry {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  override def progress = ProgressLocation(IR_StencilOffsetEntry(offset.progress, coefficient.progress))

  override def asStencilOffsetEntry = this
  override def asStencilMappingEntry = {
    def defIt = L4_ExpressionIndex((0 until numDims).toArray.map(L4_FieldIteratorAccess(_) : L4_Expression))
    L4_StencilMappingEntry(defIt, defIt + offset, coefficient)
  }

  override def numDims = offset.length
  override def colStride = Array.fill(numDims)(1.0)
}

/// L4_StencilOffsetEntry

case class L4_StencilMappingEntry(var row : L4_ExpressionIndex, var col : L4_ExpressionIndex, var coefficient : L4_Expression) extends L4_StencilEntry {

  L4_ReplaceIntWithReal.applyStandalone(row)
  L4_ReplaceIntWithReal.applyStandalone(col)

  override def prettyprint(out : PpStream) = {
    L4_GeneralSimplify.doUntilDoneStandalone(this)
    out << row << " from " << col << " with " << coefficient
  }

  override def progress = ProgressLocation(IR_StencilMappingEntry(row.progress, col.progress, coefficient.progress))

  override def asStencilOffsetEntry = {
    val offset = Duplicate(col)

    for (d <- 0 until row.length) {
      L4_ReplaceExpressions.toReplace = row.indices(d)
      L4_ReplaceExpressions.replacement = 0
      L4_ReplaceExpressions.applyStandalone(offset)
    }

    offset.indices.transform(L4_SimplifyExpression.simplifyFloatingExpr)

    L4_ReplaceRealWithInt.applyStandalone(offset)
    L4_GeneralSimplify.doUntilDoneStandalone(offset)

    L4_StencilOffsetEntry(offset.toConstIndex, coefficient)
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
      L4_ReplaceExpressions.toReplace = row.indices(d)
      L4_ReplaceExpressions.replacement = sthLarge
      L4_ReplaceExpressions.applyStandalone(stride)
    }

    stride.indices.transform(L4_SimplifyExpression.simplifyFloatingExpr)
    stride.indices.map { v =>
      val stride = v.asInstanceOf[L4_RealConstant].value / sthLarge
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
