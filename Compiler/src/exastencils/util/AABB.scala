package exastencils.util

class AABB(var lower_x : Double, var upper_x : Double,
    var lower_y : Double, var upper_y : Double,
    var lower_z : Double, var upper_z : Double) {
  def this() = this(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)

  def width(dim : Int) : Double = {
    dim match {
      case 0 => upper_x - lower_x
      case 1 => upper_y - lower_y
      case 2 => upper_z - lower_z
    }
  }
}