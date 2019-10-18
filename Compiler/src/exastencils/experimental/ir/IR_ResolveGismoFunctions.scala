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

package exastencils.experimental.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.logger.Logger

/// IR_ResolveGismoFunctions

object IR_ResolveGismoFunctions extends DefaultStrategy("ResolveGismoFunctions") {
  this += new Transformation("ResolveFunctionCalls", {
    case IR_FunctionCall(IR_UnresolvedFunctionReference("getGismoPatchIdx", _), args) =>
      if (args.nonEmpty) Logger.warn("Ignoring arguments for call to getGismoPatchIdx")

      val lexOrdering = false
      if (lexOrdering) {
        IR_IV_FragmentIndex(0) +
          (1 until Knowledge.dimensionality).map(d =>
            IR_IV_FragmentIndex(d) * (0 until d).map(Knowledge.domain_rect_numFragsTotalAsVec).product : IR_Expression).reduce(_ + _)
      } else {
        IR_IV_FragmentIndex(Knowledge.dimensionality - 1) +
          (0 until Knowledge.dimensionality - 1).map(d =>
            IR_IV_FragmentIndex(d) * (d + 1 until Knowledge.dimensionality).map(Knowledge.domain_rect_numFragsTotalAsVec).product : IR_Expression).reduce(_ + _)
      }
  })
}
