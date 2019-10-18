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

package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.datastructures.Node
import exastencils.optimization.ir._

/// IR_ExpressionIndexRange

case class IR_ExpressionIndexRange(var begin : IR_ExpressionIndex, var end : IR_ExpressionIndex) extends Node {
  def print = s"${ begin.prettyprint() } to ${ end.prettyprint() }"

  // minimum length of begin and end
  def length = math.min(begin.length, end.length)

  // evaluate the number of points in the given index range
  def getTotalSize : IR_Expression = {
    val totalSize = (end - begin).reduce(_ * _)
    IR_GeneralSimplifyWrapper.process(totalSize)
  }

  // interpret the index range as subset of a multidimensional space and linearize the given index in this subspace
  def linearizeIndex(index : IR_Index) : IR_Expression = IR_Linearization.linearizeIndex(index, end - begin)
}
