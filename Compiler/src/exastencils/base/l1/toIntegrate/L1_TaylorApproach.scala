package exastencils.base.l1.toIntegrate

/** Finite Difference Computation via Taylor series expansion
  * resulting in the solution of a eqSystem.
  *
  * Only uses central differences with odd number of grid points:
  * f(x+ih) with i_min <= i<= i_max.
  *
  * Use getWeights() to get the weights in the corresponding order
  *
  * @param  N number of gridPoints (must be odd)
  * @param  M order of the derivative
  * @param  h distance of neighboring grid points
  * @param  e order of the truncation error
  */
class L1_TaylorApproach(N : Int, M : Int, h : Double, val e : Int, val direction : Int = 0) extends L1_Approach(N, M, h) {

  assert(-1 <= direction && direction <= 1)
  assert(N == M + e || N == M + e - 1)
  assert((direction != 0) || ((N % 2) == 1))

  private val range = {
    if (direction == 1) {
      val i_max = M + e - 1;
      0 to i_max
    } else if (direction == -1) {
      val i_min = -1 * (M + e - 1);
      i_min to 0
    } else {
      val i_max = math.floor((M + e - 1) / 2).toInt
      val i_min = -i_max
      assert(i_max - i_min + 1 == N)
      i_min to i_max
    }
  }

  private val i_min = range.start
  private val i_max = range.end
  /*
  println("N:"+N)
  println("M:"+M)
  println("h:"+h)
  println("e:"+e)
  println("i_max:"+i_max)
  println("i_min:"+i_min)
  */
  assert(i_max - i_min + 1 == N)

  private val eqSystem = initEQSystem
  private val c = eqSystem.solve
  private val weights = c map { case (c_i) => (M !) * c_i / math.pow(h, M) }

  private implicit def fac(n : Int) = new {def ! = (1 /: (1 to n)) { _ * _ } }

  private def initEQSystem = {
    // todo fix range of i and computation via M and e
    val eqSys = new L1_EQSystem(N)

    for (k <- 0 until N) {
      for (i <- range)
        eqSys.set(k, i - i_min, math.pow(i, k))
      // right hand side
      if (k == M)
        eqSys.set(k, N, 1)
      else
        eqSys.set(k, N, 0)
    }
    if (direction == -1)
      eqSys.swapRows(0, -1 * i_min) // last column only has zero entries, except for the first row -> swap first and last row
    if (direction == 0)
      eqSys.swapRows(0, i_max) // middle collumn only has zero entries, except for the first row -> swap first and middle row

    eqSys
  }

  def getWeights : List[Double] = {
    if (direction == 1)
      List.fill(i_max)(0.0) ++ weights // prepend zeros for central difference stencil notation
    else if (direction == -1)
      weights ++ List.fill(-1 * i_min)(0.0) // append zeros for central difference stencil notation
    else
      weights
  }
}

class L1_EQSystem(val N : Int) {

  var data : Array[Array[Double]] = Array.fill(N, N + 1)(0.0)

  def printOut = {
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
      if (data(i)(i) == 0.0) {
        throw new RuntimeException("eqSystem not solvable")
      }
      if (data(i)(i) != 1) {
        data(i) = data(i) / (data(i)(i))
      }
      for (j <- 0 until N if (j != i && data(j) != 0.0)) {
        data(j) = data(j) - (data(i) * data(j)(i))
      }
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
