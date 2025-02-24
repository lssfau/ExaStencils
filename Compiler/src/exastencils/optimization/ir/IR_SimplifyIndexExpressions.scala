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

package exastencils.optimization.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_LinearizedFieldLikeAccess

object IR_SimplifyIndexExpressions extends DefaultStrategy("Simplify index expressions") {
  val loopExtremaCollector = new IR_LoopExtremaCollector()
  this.onBefore = () => loopExtremaCollector.reset()

  this.register(loopExtremaCollector)

  this += new Transformation("now", {
    case a : IR_ArrayAccess =>
      a.index = IR_SimplifyExpression.simplifyIntegralExpr(a.index, loopExtremaCollector.extremaMap)
      a

    case d : IR_LinearizedFieldLikeAccess =>
      d.index = IR_SimplifyExpression.simplifyIntegralExpr(d.index, loopExtremaCollector.extremaMap)
      d
  })
}
