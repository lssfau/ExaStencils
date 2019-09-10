package exastencils.domain

import scala.collection.mutable.ListBuffer

/// AABB

object AABB {
  def apply(numDims : Int) = new AABB(ListBuffer.fill(numDims)(0.0), ListBuffer.fill(numDims)(1.0))

  def apply(lower_0 : Double, upper_0 : Double)
  = new AABB(ListBuffer(lower_0), ListBuffer(upper_0))

  def apply(lower_0 : Double, lower_1 : Double, upper_0 : Double, upper_1 : Double)
  = new AABB(ListBuffer(lower_0, lower_1), ListBuffer(upper_0, upper_1))

  def apply(lower_0 : Double, lower_1 : Double, lower_2 : Double, upper_0 : Double, upper_1 : Double, upper_2 : Double)
  = new AABB(ListBuffer(lower_0, lower_1, lower_2), ListBuffer(upper_0, upper_1, upper_2))

  def apply(lower_0 : Double, lower_1 : Double, lower_2 : Double, lower_3 : Double, upper_0 : Double, upper_1 : Double, upper_2 : Double, upper_3 : Double)
  = new AABB(ListBuffer(lower_0, lower_1, lower_2, lower_3), ListBuffer(upper_0, upper_1, upper_2, upper_3))
}

case class AABB(var lower : ListBuffer[Double], upper : ListBuffer[Double]) {
  def width(dim : Int) : Double = upper(dim) - lower(dim)
}
