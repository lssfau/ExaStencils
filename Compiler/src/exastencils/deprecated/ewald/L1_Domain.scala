package exastencils.deprecated.ewald

object L1_Dimension {
  implicit def stringsToDimension(left : String, right : String) : L1_Dimension = {
    assert((left != null) && (right != null), "stringToDimension: null-String parsed")
    try {
      return new L1_Dimension(left.toDouble, right.toDouble)
    } catch {
      case e : NumberFormatException =>
        throw new IllegalArgumentException(s"Cannot convert String $left and/or  String $right to double")
    }
  }
  implicit def stringsToDimension(pair : (String, String)) : L1_Dimension = {
    assert((pair._1 != null) && (pair._2 != null), "stringToDimension: null-String parsed")
    try {
      return new L1_Dimension(pair._1.toDouble, pair._2.toDouble)
    } catch {
      case e : NumberFormatException =>
        throw new IllegalArgumentException(s"Cannot convert String $pair._1 and/or  String $pair._2 to double")
    }
  }
}

case class L1_Dimension(x0 : Double, x1 : Double) {
  require(x0 < x1, "Left dimension boundary greater than right " + this.toString)
  override def toString = s"[$x0,$x1]"
}

class L1_Domain(val dimensions : Seq[L1_Dimension]) {
  lazy val dimCount = dimensions.length
  def this(dimension : L1_Dimension) = this(Seq(dimension))
  def this() = this(Seq())

  def +(other : L1_Domain) : L1_Domain = new L1_Domain(this.dimensions ++ other.dimensions)
  def +(dim : L1_Dimension) : L1_Domain = new L1_Domain(this.dimensions :+ dim)

  override def toString : String = "Domain(" + dimensions.mkString(", ") + ")"
}

