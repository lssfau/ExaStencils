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
