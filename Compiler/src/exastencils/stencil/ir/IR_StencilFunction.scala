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

package exastencils.stencil.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.operator.ir._
import exastencils.util.ir.IR_StackCollector

/// IR_StencilFunctionReference

case class IR_StencilFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference {}

/// IR_ResolveStencilFunction

object IR_ResolveStencilFunction extends DefaultStrategy("Resolve stencil functions") {
  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case fctCall @ IR_FunctionCall(IR_StencilFunctionReference(fctName, _), args) =>
      fctName match {
        // diag function
        case "diag" =>
          args(0) match {
            case access : IR_StencilAccess =>
              // stencil access => find entry with 0 offset and return coefficient
              val centralOffset = IR_ConstIndex(Array.fill(Knowledge.dimensionality)(0))
              Duplicate(access.target.findStencilEntry(centralOffset).get.coefficient)

            case access : IR_StencilFieldAccess =>
              // stencil field access => find entry with 0 offset in linked stencil and compile field access
              val index = Duplicate(access.index)
              val centralOffset = IR_ConstIndex(Array.fill(Knowledge.dimensionality)(0))
              index.indices :+= (access.stencilField.findStencilEntryIndex(centralOffset).get : IR_Expression)
              index.indices :+= (0 : IR_Expression) // honor matrix data type

              IR_FieldLikeAccess(access.field, Duplicate(access.slot), Duplicate(access.fragIdx), index)

            case _ =>
              Logger.warn("diag with unknown arg " + args(0))
              IR_FunctionCall(fctName, args)
          }

        // diag_inv
        case "diag_inv" =>
          // FIXME: check datatype -> 'real' invert for matrices
          fctCall.function.name = "diag"
          1.0 / fctCall

        case _ =>
          Logger.warn("stencil function with unknown name " + fctName)
          IR_FunctionCall(fctName, args)
      }
  })
}
