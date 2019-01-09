package exastencils.util

object MathHelper {
  def gcd(x : Long, y : Long) : Long = {
    var a : Long = x
    var b : Long = y
    while (b != 0) {
      val h = a % b
      a = b
      b = h
    }
    math.abs(a)
  }
}
