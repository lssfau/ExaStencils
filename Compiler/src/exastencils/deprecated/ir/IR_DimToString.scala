package exastencils.deprecated.ir

@deprecated("to be replaced by something reasonable", "05.10.16")
object IR_DimToString extends (Int => String) {
  // FIXME: this is named inappropriately; move this to a global variable manager as it becomes available; rename to i_x after checking where x, etc are used explicitly
  override def apply(dim : Int) : String = {
    dim match {
      case 0 => "x"
      case 1 => "y"
      case 2 => "z"
      case 3 => "w"
      case 4 => "v"
      case 5 => "u"
      case _ => "UNKNOWN"
    }
  }
}
