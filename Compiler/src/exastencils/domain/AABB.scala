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

package exastencils.domain

import scala.collection.mutable.ListBuffer

/// AABB

object AABB {
  def apply(numDims : Int) = new AABB(ListBuffer.fill(numDims)(0.0), ListBuffer.fill(numDims)(1.0))

  def apply(lower_0 : Double, upper_0 : Double)
  = new AABB(ListBuffer(lower_0), ListBuffer(upper_0))

  def apply(lower_0 : Double, lower_1 : Double, upper_0 : Double, upper_1 : Double)
  = new AABB(ListBuffer(lower_0, lower_1), ListBuffer(upper_0, upper_1))

  def apply(lower_0 : Double, lower_1 : Double, lower_2 : Double, upper_0 : Double, upper_1 : Double, upper_2 : Double)
  = new AABB(ListBuffer(lower_0, lower_1, lower_2), ListBuffer(upper_0, upper_1, upper_2))

  def apply(lower_0 : Double, lower_1 : Double, lower_2 : Double, lower_3 : Double, upper_0 : Double, upper_1 : Double, upper_2 : Double, upper_3 : Double)
  = new AABB(ListBuffer(lower_0, lower_1, lower_2, lower_3), ListBuffer(upper_0, upper_1, upper_2, upper_3))
}

case class AABB(var lower : ListBuffer[Double], upper : ListBuffer[Double]) {
  def width(dim : Int) : Double = upper(dim) - lower(dim)
}
