package exastencils.operator.l2

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.operator.l3._
import exastencils.optimization.l2._
import exastencils.prettyprinting._

/// L2_StencilEntry

abstract class L2_StencilEntry extends L2_Node with L2_Progressable with PrettyPrintable {
  override def progress : L3_StencilEntry
}

/// L2_StencilOffsetEntry

case class L2_StencilOffsetEntry(var offset : L2_Index, var coefficient : L2_Expression) extends L2_StencilEntry {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  override def progress = L3_StencilEntry(offset.progress, coefficient.progress)
}

/// L2_StencilOffsetEntry

case class L2_StencilMappingEntry(var row : L2_ExpressionIndex, var col : L2_ExpressionIndex, var coefficient : L2_Expression) extends L2_StencilEntry {
  override def prettyprint(out : PpStream) = out << row << " from " << col << " with " << coefficient

  override def progress = {
    // FIXME: specialized L3 class L3_StencilEntry(row.progress, col.progress, coefficient.progress)
    toStencilOffsetEntry.progress
  }

  def toStencilOffsetEntry : L2_StencilOffsetEntry = {
    val offset = Duplicate(col)

    for (d <- 0 until row.length) {
      L2_Replace.toReplace = row.indices(d)
      L2_Replace.replacement = 0
      L2_Replace.applyStandalone(offset)
    }

    offset.indices.transform(L2_SimplifyExpression.simplifyIntegralExpr)

    L2_GeneralSimplify.applyStandalone(offset)

    L2_StencilOffsetEntry(offset, coefficient)
  }
}

object L2_Replace extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : L2_Expression = L2_NullExpression
  var replacement : L2_Expression = L2_NullExpression

  this += new Transformation("Search and replace", {
    case e : L2_Expression if e == toReplace => Duplicate(replacement)
  }, false)
}
