package exastencils.polyhedron.exploration

import exastencils.polyhedron.Isl.TypeAliases._

object Util {
  def gcd(x : Int, y : Int) : Int = {
    var a : Int = x
    var b : Int = y
    while (a != 0) {
      val h = b % a
      b = a
      a = h
    }
    return math.abs(b)
  }

  def vectorToArray(vector : Vector[BigInt]) : Array[Int] = {
    val point : Array[Int] = new Array[Int](vector.length)
    for ((bI, i) <- vector.view.zipWithIndex)
      point(i) = bI.intValue()
    return point
  }

  private def maybeCreateArray(a : Array[Int], len : Int) : (Array[Int], Int) = {
    if (a == null)
      return (new Array[Int](len), len)
    else
      return (a, math.min(len, a.length))
  }

  def addArrayPW(a1 : Array[Int], a2 : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, math.min(a1.length, a2.length))

    var i : Int = 0
    while (i < length) {
      res(i) = a1(i) + a2(i)
      i += 1
    }

    return res
  }

  def subArrayPW(a1 : Array[Int], a2 : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, math.min(a1.length, a2.length))

    var i : Int = 0
    while (i < length) {
      res(i) = a1(i) - a2(i)
      i += 1
    }

    return res
  }

  def negateArrayPW(in : Array[Int], out : Array[Int]) : Array[Int] = {
    return mulArrayPW(-1, in, out)
  }

  def mulArrayPW(c : Int, in : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, in.length)

    var i : Int = 0
    while (i < length) {
      res(i) = c * in(i)
      i += 1
    }

    return res
  }

  def divEArrayPW(in : Array[Int], c : Int, out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, in.length)

    var i : Int = 0
    while (i < length) {
      if (in(i) % c != 0)
        return null // not exactly dividable... no result
      res(i) = in(i) / c
      i += 1
    }

    return res
  }

  def fstNZero(a : Array[Int], n : Int) : Boolean = {
    var i : Int = 0
    while (i < n) {
      if (a(i) != 0)
        return false
      i += 1
    }
    return true
  }

  def reduceByGCD(in : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, in.length)

    // find gcd first
    var gcd : Int = in(0)
    var i : Int = 1
    while (i < length) {
      gcd = this.gcd(gcd, in(i))
      i += 1
    }

    // divide everything if gcd is not 1 (or 0)
    if (gcd != 1 && gcd != 0) {
      i = 0
      while (i < length) {
        res(i) = in(i) / gcd
        i += 1
      }
    }

    return res
  }

  def islSetContains(set : isl.Set, pt : Array[Int]) : Boolean = {
    var islPt : isl.Set = set
    val ctx = set.getCtx()
    for ((p, i) <- pt.view.zipWithIndex)
      islPt = islPt.fixVal(T_SET, i, isl.Val.intFromSi(ctx, p))

    return !islPt.isEmpty()
  }
}
