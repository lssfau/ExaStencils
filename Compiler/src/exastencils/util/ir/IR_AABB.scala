package exastencils.util.ir

import exastencils.base.ir._
import exastencils.prettyprinting._

/// IR_AABB

case class IR_AABB(var lower : Array[Double], var upper : Array[Double]) extends IR_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"

  def numDims = math.min(lower.length, upper.length)

  def width() = upper.zip(lower).map(i => i._1 - i._2)
  def width(dim : Int) = upper(dim) - lower(dim)
}
