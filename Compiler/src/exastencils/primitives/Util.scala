package exastencils.primitives

object dirToString extends (Int => String) {
  def apply(dim : Int) : String = {
    return dim match {
      case -1 => "N"
      case 0  => "0"
      case 1  => "P"
      case _  => "UNKNOWN"
    }
  }
}