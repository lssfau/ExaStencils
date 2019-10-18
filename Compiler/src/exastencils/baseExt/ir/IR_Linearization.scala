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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

object IR_Linearization {
  def linearizeIndex(index : IR_Index, strides : IR_Index) : IR_Expression = {
    if (strides.length() != index.length()) Logger.warn(s"Index with dimensionality ${ index.length() } does not match strides with dimensionality ${ strides.length() }")

    val ret = (0 until math.min(strides.length(), index.length())).map(dim => {
      val stride = (0 until dim).map(strides.toExpressionIndex(_)).fold(1 : IR_Expression)(_ * _)
      index.toExpressionIndex(dim) * stride
    }).fold(0 : IR_Expression)(_ + _)

    IR_SimplifyExpression.simplifyIntegralExpr(ret)
  }
}
