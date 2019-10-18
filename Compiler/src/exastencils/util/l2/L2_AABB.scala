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

package exastencils.util.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._
import exastencils.util.l3.L3_AABB

/// L2_AABB

case class L2_AABB(var lower : Array[Double], var upper : Array[Double]) extends L2_Node with L2_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"
  override def progress = ProgressLocation(L3_AABB(lower, upper))

  def numDims = math.min(lower.length, upper.length)

  def width() = upper.zip(lower).map(i => i._1 - i._2)
  def width(dim : Int) = upper(dim) - lower(dim)
}
