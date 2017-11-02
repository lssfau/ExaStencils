package exastencils.util.l1

import exastencils.base.l1._
import exastencils.prettyprinting._
import exastencils.util.l2.L2_AABB

/// L1_AABB

case class L1_AABB(var lower : Array[Double], var upper : Array[Double]) extends L1_Node with L1_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"
  override def progress = L2_AABB(lower, upper)

  def numDims = math.min(lower.length, upper.length)

  def width() = upper.zip(lower).map(i => i._1 - i._2)
  def width(dim : Int) = upper(dim) - lower(dim)
}
