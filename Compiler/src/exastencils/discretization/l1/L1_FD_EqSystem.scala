package exastencils.discretization.l1

import exastencils.logger.Logger

class L1_FD_EqSystem(val N : Int) {
  var data : Array[Array[Double]] = Array.fill(N, N + 1)(0.0)

  def printOut() = {
    for (y <- 0 to N) {
      print("( ")
      for (x <- 0 until N) {
        print(data(y)(x) + " ")
      }
      print(")")
    }
  }

  def set(y : Int, x : Int, value : Double) = {
    assert(0 <= y && y < N, s"Index y out of bounds (y=$y, N = $N)")
    assert(0 <= x && x < N + 1, s"Index x out of bounds (x=$x)")
    data(y)(x) = value
  }

  private implicit def mul(D : Array[Double]) = new {def *(k : Double) = D map { d => d * k } }
  private implicit def div(D : Array[Double]) = new {def /(k : Double) = D map { d => d / k } }
  private implicit def add(D : Array[Double]) = new {def +(E : Array[Double]) = D zip E map { case (a, b) => a + b } }
  private implicit def sub(D : Array[Double]) = new {def -(E : Array[Double]) = D zip E map { case (a, b) => a - b } }

  def solve() : List[Double] = {
    for (i <- 0 until N) {
      handleZeroEntry(i)
      if (data(i)(i) == 0.0) Logger.error("eqSystem not solvable")
      if (data(i)(i) != 1)
        data(i) /= data(i)(i)
      for (j <- 0 until N if j != i && data(j)(i) != 0.0)
        data(j) -= (data(i) * data(j)(i))
    }

    (for (k <- 0 until N) yield data(k)(N)).toList
  }

  private def handleZeroEntry(i : Int) = {
    if (data(i)(i) == 0.0) {
      for (j <- i + 1 to N) {
        if (data(j)(j) != 0.0) {
          swapRows(i, j)
        }
      }
    }
  }

  def swapRows(i : Int, j : Int) = {
    val temp = data(i)
    data(i) = data(j)
    data(j) = temp
  }
}
