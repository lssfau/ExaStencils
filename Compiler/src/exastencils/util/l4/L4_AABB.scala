package exastencils.util.l4

import exastencils.base.l4._
import exastencils.prettyprinting._
import exastencils.util.ir.IR_AABB

/// L4_AABB

case class L4_AABB(var lower : Array[Double], var upper : Array[Double]) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"
  override def progress = IR_AABB(lower, upper)

  def numDims = math.min(lower.length, upper.length)

  def width() = upper.zip(lower).map(i => i._1 - i._2)
  def width(dim : Int) = upper(dim) - lower(dim)
}
