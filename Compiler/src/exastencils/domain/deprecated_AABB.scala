package exastencils.domain

/// deprecated_AABB

object deprecated_AABB {
  def apply() = new deprecated_AABB(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)
}

// TODO: refactor to be independent of dimensionality
case class deprecated_AABB(
    var lower_x : Double, var upper_x : Double,
    var lower_y : Double, var upper_y : Double,
    var lower_z : Double, var upper_z : Double) {

  def lower(dim : Int) : Double = {
    dim match {
      case 0 => lower_x
      case 1 => lower_y
      case 2 => lower_z
    }
  }

  def upper(dim : Int) : Double = {
    dim match {
      case 0 => upper_x
      case 1 => upper_y
      case 2 => upper_z
    }
  }

  def width(dim : Int) : Double = {
    dim match {
      case 0 => upper_x - lower_x
      case 1 => upper_y - lower_y
      case 2 => upper_z - lower_z
    }
  }
}