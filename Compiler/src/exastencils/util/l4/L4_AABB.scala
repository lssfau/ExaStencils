package exastencils.util.l4

import exastencils.base.l4._
import exastencils.prettyprinting._
import exastencils.util.ir.IR_AABB

/// L4_AABB

case class L4_AABB(var lower : L4_ExpressionIndex, var upper : L4_ExpressionIndex) extends L4_Node with L4_Progressable with PrettyPrintable {

  L4_ReplaceIntWithReal.applyStandalone(lower)
  L4_ReplaceIntWithReal.applyStandalone(upper)

  override def prettyprint(out : PpStream) = out << lower << " to " << upper
  override def progress = IR_AABB(lower.progress, upper.progress)

  def numDims = math.min(lower.length, upper.length)

  def width() = upper - lower
  def width(dim : Int) = upper(dim) - lower(dim)
}
