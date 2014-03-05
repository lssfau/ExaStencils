package exastencils.primitives

object dimToString extends (Int => String) {
  def apply(dim : Int) : String = {
    return dim match {
      case 0 => "x";
      case 1 => "y";
      case 2 => "z";
      case _ => "UNKNOWN";
    }
  }
};

object dimToString_Steffan extends (Int => String) { // FIXME: required for compatibility with Harald's naming conventions; replace with regular mapping
  def apply(dim : Int) : String = {
    return dim match {
      case 0 => "i0";
      case 1 => "i1";
      case 2 => "i2";
      case _ => "UNKNOWN";
    }
  }
};

object dirToString extends (Int => String) {
  def apply(dim : Int) : String = {
    return dim match {
      case -1 => "N";
      case 0  => "0";
      case 1  => "P";
      case _  => "UNKNOWN";
    }
  }
};