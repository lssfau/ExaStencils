package exastencils.primitives

object dimToString extends (Int => String) {
  def apply(dim : Int) : String = {
    return dim match {
      case 0 => "x"
      case 1 => "y"
      case 2 => "z"
      case _ => "UNKNOWN"
    }
  }
}

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