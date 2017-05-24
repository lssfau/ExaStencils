package exastencils.util.l3

import exastencils.base.l3._
import exastencils.prettyprinting._
import exastencils.util.l4.L4_AABB

/// L3_AABB

case class L3_AABB(var lower : L3_ExpressionIndex, var upper : L3_ExpressionIndex) extends L3_Node with L3_Progressable with PrettyPrintable {

  L3_ReplaceIntWithReal.applyStandalone(lower)
  L3_ReplaceIntWithReal.applyStandalone(upper)

  override def prettyprint(out : PpStream) = out << lower << " to " << upper
  override def progress = L4_AABB(lower.progress, upper.progress)

  def numDims = math.min(lower.length, upper.length)

  def width() = upper - lower
  def width(dim : Int) = upper(dim) - lower(dim)
}
