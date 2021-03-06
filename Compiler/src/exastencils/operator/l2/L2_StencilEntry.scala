//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.operator.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.baseExt.l2.L2_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.operator.l3._
import exastencils.optimization.l2._
import exastencils.prettyprinting._
import exastencils.util.l2._

/// L2_StencilEntry

abstract class L2_StencilEntry extends L2_Node with L2_Progressable with PrettyPrintable {
  override def progress : L3_StencilEntry

  def asStencilOffsetEntry : L2_StencilOffsetEntry
  def asStencilMappingEntry : L2_StencilMappingEntry

  def numDims : Int
  def colStride : Array[Double]

  var coefficient : L2_Expression
}

/// L2_StencilOffsetEntry

case class L2_StencilOffsetEntry(var offset : L2_ConstIndex, var coefficient : L2_Expression) extends L2_StencilEntry {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  override def progress = ProgressLocation(L3_StencilOffsetEntry(offset.progress, coefficient.progress))

  override def asStencilOffsetEntry = this
  override def asStencilMappingEntry = {
    def defIt = L2_ExpressionIndex((0 until numDims).toArray.map(L2_FieldIteratorAccess(_) : L2_Expression))
    L2_StencilMappingEntry(defIt, defIt + offset, coefficient)
  }

  override def numDims = offset.length
  override def colStride = Array.fill(numDims)(1.0)
}

/// L2_StencilOffsetEntry

case class L2_StencilMappingEntry(var row : L2_ExpressionIndex, var col : L2_ExpressionIndex, var coefficient : L2_Expression) extends L2_StencilEntry {

  L2_ReplaceIntWithReal.applyStandalone(row)
  L2_ReplaceIntWithReal.applyStandalone(col)

  override def prettyprint(out : PpStream) = {
    L2_GeneralSimplify.doUntilDoneStandalone(this)
    out << row << " from " << col << " with " << coefficient
  }

  override def progress = ProgressLocation(L3_StencilMappingEntry(row.progress, col.progress, coefficient.progress))

  override def asStencilOffsetEntry = {
    val offset = Duplicate(col)

    for (d <- 0 until row.length) {
      L2_ReplaceExpressions.toReplace = row.indices(d)
      L2_ReplaceExpressions.replacement = 0
      L2_ReplaceExpressions.applyStandalone(offset)
    }

    offset.indices.transform(L2_SimplifyExpression.simplifyFloatingExpr)

    L2_ReplaceRealWithInt.applyStandalone(offset)
    L2_GeneralSimplify.doUntilDoneStandalone(offset)

    L2_StencilOffsetEntry(offset.toConstIndex, coefficient)
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
      L2_ReplaceExpressions.toReplace = row.indices(d)
      L2_ReplaceExpressions.replacement = sthLarge
      L2_ReplaceExpressions.applyStandalone(stride)
    }

    stride.indices.transform(L2_SimplifyExpression.simplifyFloatingExpr)
    stride.indices.map { v =>
      val stride = v.asInstanceOf[L2_RealConstant].value / sthLarge
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
