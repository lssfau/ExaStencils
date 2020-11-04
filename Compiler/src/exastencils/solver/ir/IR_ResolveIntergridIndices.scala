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

package exastencils.solver.ir

import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.operator.ir.IR_StencilFieldAccess
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.util.ir._

/// IR_ResolveIntergridIndices

object IR_ResolveIntergridIndices extends DefaultStrategy("Resolve indices in operations between two grid levels") {
  val collector = new IR_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  var overrideLevel: Option[Int] = None

  def inLevelScope = overrideLevel.isDefined || collector.inLevelScope

  def curLevel = overrideLevel.getOrElse(collector.getCurrentLevel)

  // TODO: checking for being inside a valid level scope is currently required for setting up geometric information of grids with varying cell sizes
  // TODO: think about if this case (access outside of a loop) should be supported

  this += new Transformation("ModifyIndices", {
    case fct: IR_FunctionCall if "changeLvlAndIndices" == fct.name =>
      // extract information from special function call
      val fieldAccess = fct.arguments(0).asInstanceOf[IR_FieldAccess]
      Logger.warn("Performing index adaptation for " + fieldAccess.field.codeName)

      // adapt per dimension / (n+1)d is reserved
      for (dim <- 0 until Knowledge.dimensionality) {
        val idxAdaption = fct.arguments(2 + dim)

        // insert old index into index adaptation function
        IR_ReplaceVariableAccess.replace = Map("i" -> Duplicate(fieldAccess.index(dim)))
        val newIdx = Duplicate(idxAdaption)
        IR_ReplaceVariableAccess.applyStandalone(newIdx)

        // overwrite old index
        fieldAccess.index(dim) = newIdx
      }

      fieldAccess

    case access: IR_FieldAccess if inLevelScope && IR_SimplifyExpression.evalIntegral(access.level) < curLevel =>
      val fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = fieldAccess.index(i) / 2
      fieldAccess

    case access: IR_FieldAccess if inLevelScope && IR_SimplifyExpression.evalIntegral(access.level) > curLevel =>
      val fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = 2 * fieldAccess.index(i)
      fieldAccess

    case access: IR_StencilFieldAccess if inLevelScope && IR_SimplifyExpression.evalIntegral(access.level) < curLevel =>
      val stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = stencilFieldAccess.index(i) / 2
      stencilFieldAccess

    case access: IR_StencilFieldAccess if inLevelScope && IR_SimplifyExpression.evalIntegral(access.level) > curLevel =>
      val stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = 2 * stencilFieldAccess.index(i)
      stencilFieldAccess
  }, false /* don't do this recursively -> avoid double adaptation for cases using special functions */)
}

