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
import exastencils.datastructures._
import exastencils.scheduling.NoStrategyWrapper

/// IR_InferDiagAndInverseCallDataTypes

object IR_InferDiagAndInverseCallDataTypes extends DefaultStrategy("InferDiagAndInverseCallDataTypes") {
  var _changed = 0

  var fcts = List("diag", "inverse")

  def doUntilDone(node : Option[Node] = None) = {
    do {
      _changed = 0
      apply(node)
    } while (_changed > 0)
  }

  this += Transformation("do", {
    case call @ IR_FunctionCall(ref : IR_UnresolvedFunctionReference, params)
      if fcts.contains(ref.name) && call.datatype == IR_UnknownDatatype && params.head.datatype != IR_UnknownDatatype =>

      _changed += 1
      call.function = IR_PlainInternalFunctionReference(ref.name, params.head.datatype)
      call
  })
}

/// IR_InferDiagAndInverseCallDataTypesWrapper

object IR_InferDiagAndInverseCallDataTypesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => IR_InferDiagAndInverseCallDataTypes.doUntilDone()
}
