package exastencils.discretization.l1

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
class L1_FD_TaylorApproach(N : Int, M : Int, h : Double, val e : Int, val direction : Int = 0) extends L1_FD_Approach(N, M, h) {

  assert(-1 <= direction && direction <= 1)
  assert(N == M + e || N == M + e - 1)
  assert((direction != 0) || ((N % 2) == 1))

  private val range = {
    if (direction == 1) {
      val i_max = M + e - 1
      0 to i_max
    } else if (direction == -1) {
      val i_min = -1 * (M + e - 1)
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
  assert(i_max - i_min + 1 == N)

  private val eqSystem = initEQSystem()
  private val c = eqSystem.solve()
  private val weights = c.map(c_i => fac(M) * c_i / math.pow(h, M))

  private def fac(n : Int) = (1 /: (1 to n)) { _ * _ }

  private def initEQSystem() = {
    // todo fix range of i and computation via M and e
    val eqSys = new L1_FD_EqSystem(N)

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
      eqSys.swapRows(0, i_max) // middle column only has zero entries, except for the first row -> swap first and middle row

    eqSys
  }

  override def getWeights : List[Double] = weights
  override def getOffsets : List[Int] = range.toList
}
