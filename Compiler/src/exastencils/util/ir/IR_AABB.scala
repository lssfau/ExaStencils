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

package exastencils.util.ir

import exastencils.base.ir._
import exastencils.prettyprinting._

/// IR_AABB

case class IR_AABB(var lower : Array[Double], var upper : Array[Double]) extends IR_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"

  def numDims = math.min(lower.length, upper.length)

  def width() = upper.zip(lower).map(i => i._1 - i._2)
  def width(dim : Int) = upper(dim) - lower(dim)
}
