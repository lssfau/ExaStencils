package exastencils.base.l1.toIntegrate

/** Finite Difference Computation via Lagrange Polynomial
  *
  * Only uses central differences (odd and even number of grid points).
  * Can result in discretizations that witch values based on x0
  *
  * Use getWeights() to get the finite difference weights
  *
  * @param  N  number of gridPoints
  * @param  M  order of derivative
  * @param  h  distance of neighboring grid points
  * @param  x0 finite difference discretization point (for N = M+1 unimportant)
  */
class L1_LagrangeApproach(N : Int, M : Int, h : Double, val x0 : Double = 0.0) extends L1_Approach(N, M, h) {
  assert(N > M)
  private val gridPoints = initGridPoints
  private val laWeights = lagrangeWeights()
  private var weights : List[Double] = setX0(x0)

  private def _initGridPoints = {
    val range = if ((N % 2) == 1) List.range(-(N - 1) / 2, (N - 1) / 2 + 1)
    else List.range(-N / 2, N / 2 + 1)
    range map { _ * h }
  }

  private def initGridPoints = {
    if ((N % 2) == 1) {
      val i_max = (N - 1) / 2
      for (i <- -i_max to i_max) yield x0 + i * h
    } else {
      for (i <- 0 until N) yield x0 + i * h - (N / 2) + (h / 2)
    }
  }

  private def lagrangeWeights() = {
    def product(i : Int) = {
      val w_i = for (j <- gridPoints.indices if (j != i)) yield gridPoints(i) - gridPoints(j)
      1.0 / (w_i reduce { _ * _ })
    }
    for (i <- gridPoints.indices) yield product(i)
  }

  private def multBinom(a : Array[Double], z_k : Double) = {
    val b_0 = -z_k * a(0)
    val b = for (m <- 1 until a.length) yield -z_k * a(m) + a(m - 1)
    b_0 +: b.toArray
  }

  private def convolve(a : Array[Double], b : Array[Double]) : Array[Double] = {
    def sum(m : Int) = {
      val c_m = for (l <- 0 to m) yield a(l) * b(m - l)
      c_m reduce { _ + _ }
    }
    assert(a.length == b.length)
    val c = for (m <- a.indices) yield sum(m)
    c.toArray
  }

  private implicit def fac(n : Int) = new {def ! = (1 /: (1 to n)) { _ * _ } }

  def setX0(x0 : Double) = {
    val _gridPoints = gridPoints map { x_k => x_k - x0 }
    var R = Array.fill(N + 2, M + 1)(0.0)
    var L = Array.fill(N + 2, M + 1)(0.0)
    R(N + 1)(0) = 1
    L(0)(0) = 1
    for (k <- 1 to N) { L(k) = multBinom(L(k - 1), _gridPoints(k - 1)) }
    for (k <- N to 1 by -1) { R(k) = multBinom(R(k + 1), _gridPoints(k - 1)) }
    val c = for (k <- 1 to N) yield convolve(L(k - 1), R(k + 1))
    (for (k <- 0 to N - 1) yield (M !) * laWeights(k) * c(k)(M)).toList
  }

  def computeWeightsAt(x : Double) : Unit = { weights = setX0(x) }

  override def getWeights = weights
}

