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

/** basic interface for a finite difference approach
  *
  * getWeights() is used to get the finite difference weights
  *
  * @param  N number of gridPoints
  * @param  M order of derivative
  * @param  h distance of neighboring grid points
  */
abstract class L1_FD_Approach(val N : Int, val M : Int, val h : Double) {
  assert(N >= 1)
  assert(M >= 0)
  assert(h > 0)

  def getWeights : List[Double]
  def getOffsets : List[Int]
}
