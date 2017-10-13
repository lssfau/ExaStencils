package exastencils.operator.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.prettyprinting._
import exastencils.util.ir._

/// IR_StencilEntry

abstract class IR_StencilEntry extends IR_Node with PrettyPrintable {
  def datatype = coefficient.datatype

  def asStencilOffsetEntry : IR_StencilOffsetEntry
  def asStencilMappingEntry : IR_StencilMappingEntry

  def numDims : Int
  def colStride : Array[Double]

  var coefficient : IR_Expression
}

/// IR_StencilOffsetEntry

case class IR_StencilOffsetEntry(var offset : IR_ConstIndex, var coefficient : IR_Expression) extends IR_StencilEntry {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient

  override def asStencilOffsetEntry = this
  override def asStencilMappingEntry = {
    def defIt = IR_ExpressionIndex((0 until numDims).toArray.map(IR_FieldIteratorAccess(_) : IR_Expression))
    IR_StencilMappingEntry(defIt, defIt + offset, coefficient)
  }

  override def numDims = offset.length
  override def colStride = Array.fill(numDims)(1.0)
}

/// IR_StencilOffsetEntry

case class IR_StencilMappingEntry(var row : IR_ExpressionIndex, var col : IR_ExpressionIndex, var coefficient : IR_Expression) extends IR_StencilEntry {

  IR_ReplaceIntWithReal.applyStandalone(row)
  IR_ReplaceIntWithReal.applyStandalone(col)

  override def prettyprint(out : PpStream) = out << row << " from " << col << " with " << coefficient

  override def asStencilOffsetEntry = {
    val offset = Duplicate(col)

    for (d <- 0 until row.length) {
      IR_ReplaceExpressions.toReplace = row.indices(d)
      IR_ReplaceExpressions.replacement = 0
      IR_ReplaceExpressions.applyStandalone(offset)
    }

    offset.indices.transform(IR_SimplifyExpression.simplifyFloatingExpr)

    IR_ReplaceRealWithInt.applyStandalone(offset)
    IR_GeneralSimplify.doUntilDoneStandalone(offset)

    IR_StencilOffsetEntry(offset.toConstIndex, coefficient)
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
      IR_ReplaceExpressions.toReplace = row.indices(d)
      IR_ReplaceExpressions.replacement = sthLarge
      IR_ReplaceExpressions.applyStandalone(stride)
    }

    stride.indices.transform(IR_SimplifyExpression.simplifyFloatingExpr)
    stride.indices.map { v =>
      val stride = v.asInstanceOf[IR_RealConstant].value / sthLarge
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
