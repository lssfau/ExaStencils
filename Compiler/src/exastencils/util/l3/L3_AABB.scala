package exastencils.util.l3

import exastencils.base.l3._
import exastencils.prettyprinting._
import exastencils.util.l4.L4_AABB

/// L3_AABB

case class L3_AABB(var lower : Array[Double], var upper : Array[Double]) extends L3_Node with L3_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"
  override def progress = L4_AABB(lower, upper)

  def numDims = math.min(lower.length, upper.length)

  def width() = upper.zip(lower).map(i => i._1 - i._2)
  def width(dim : Int) = upper(dim) - lower(dim)
}
