//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.discretization.l1

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
class L1_FD_LagrangeApproach(N : Int, M : Int, h : Double, val x0 : Double = 0.0) extends L1_FD_Approach(N, M, h) {
  assert(N > M)
  private val gridPoints = initGridPoints
  private val laWeights = lagrangeWeights()
  private var weights : List[Double] = setX0(x0)

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
      val w_i = for (j <- gridPoints.indices if j != i) yield gridPoints(i) - gridPoints(j)
      1.0 / w_i.product
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
      c_m.sum
    }

    assert(a.length == b.length)
    val c = for (m <- a.indices) yield sum(m)
    c.toArray
  }

  private def fac(n : Int) = (1 /: (1 to n)) { _ * _ }

  def setX0(x0 : Double) = {
    val _gridPoints = gridPoints map { x_k => x_k - x0 }
    val R = Array.fill(N + 2, M + 1)(0.0)
    val L = Array.fill(N + 2, M + 1)(0.0)
    R(N + 1)(0) = 1
    L(0)(0) = 1
    for (k <- 1 to N) { L(k) = multBinom(L(k - 1), _gridPoints(k - 1)) }
    for (k <- N to 1 by -1) { R(k) = multBinom(R(k + 1), _gridPoints(k - 1)) }
    val c = for (k <- 1 to N) yield convolve(L(k - 1), R(k + 1))
    (for (k <- 0 until N) yield fac(M) * laWeights(k) * c(k)(M)).toList
  }

  def computeWeightsAt(x : Double) : Unit = { weights = setX0(x) }

  override def getWeights : List[Double] = weights
  override def getOffsets : List[Int] = gridPoints.toList.map(_.toInt)
}
