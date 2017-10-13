package exastencils.util.l2

import exastencils.base.l2._
import exastencils.prettyprinting._
import exastencils.util.l3.L3_AABB

/// L2_AABB

case class L2_AABB(var lower : L2_ExpressionIndex, var upper : L2_ExpressionIndex) extends L2_Node with L2_Progressable with PrettyPrintable {

  L2_ReplaceIntWithReal.applyStandalone(lower)
  L2_ReplaceIntWithReal.applyStandalone(upper)

  override def prettyprint(out : PpStream) = out << lower << " to " << upper
  override def progress = L3_AABB(lower.progress, upper.progress)

  def numDims = math.min(lower.length, upper.length)

  def width() = upper - lower
  def width(dim : Int) = upper(dim) - lower(dim)
}
