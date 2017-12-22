package exastencils.util.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._
import exastencils.util.l3.L3_AABB

/// L2_AABB

case class L2_AABB(var lower : Array[Double], var upper : Array[Double]) extends L2_Node with L2_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"
  override def progress = ProgressLocation(L3_AABB(lower, upper))

  def numDims = math.min(lower.length, upper.length)

  def width() = upper.zip(lower).map(i => i._1 - i._2)
  def width(dim : Int) = upper(dim) - lower(dim)
}
