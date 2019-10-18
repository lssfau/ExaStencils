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

package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_FieldIteratorAccess
import exastencils.grid.l3._

object L3_DefaultRestriction {
  def stencilNodeLinear(level : Int) : L3_Stencil = {
    def it = L3_FieldIteratorAccess(0)
    def itAsIndex = L3_ExpressionIndex(it)
    L3_Stencil("linearNode", level, 1, Array(2.0), ListBuffer(
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it - 1.0), 0.25),
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 0.0), 0.5),
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 1.0), 0.25)))
  }

  def stencilCellLinear(level : Int) : L3_Stencil = {
    def it = L3_FieldIteratorAccess(0)
    def itAsIndex = L3_ExpressionIndex(it)
    L3_Stencil("linearCell", level, 1, Array(2.0), ListBuffer(
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 0.0), 0.5),
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 1.0), 0.5)))
  }

  def stencilNodeIntegralLinear(level : Int) : L3_Stencil = {
    def it = L3_FieldIteratorAccess(0)
    def itAsIndex = L3_ExpressionIndex(it)
    L3_Stencil("integralLinearNode", level, 1, Array(2.0), ListBuffer(
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it - 1.0), 0.5),
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 0.0), 1.0),
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 1.0), 0.5)))
  }

  def stencilCellIntegralLinear(level : Int) : L3_Stencil = {
    def it = L3_FieldIteratorAccess(0)
    def itAsIndex = L3_ExpressionIndex(it)
    L3_Stencil("integralLinearCell", level, 1, Array(2.0), ListBuffer(
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 0.0), 1.0),
      L3_StencilMappingEntry(itAsIndex, L3_ExpressionIndex(2.0 * it + 1.0), 1.0)))
  }

  def generate(name : String, level : Int, numDims : Int, localization : L3_Localization, interpolation : String) : L3_Stencil = {
    val stencils = (0 until numDims).map(dim => {
      interpolation match {
        case "linear" => localization match {
          case L3_AtNode                                  => stencilNodeLinear(level)
          case L3_AtCellCenter                            => stencilCellLinear(level)
          case L3_AtFaceCenter(faceDim) if faceDim == dim => stencilNodeLinear(level)
          case L3_AtFaceCenter(_)                         => stencilCellLinear(level)
        }

        case "integral_linear" => localization match {
          case L3_AtNode                                  => stencilNodeIntegralLinear(level)
          case L3_AtCellCenter                            => stencilCellIntegralLinear(level)
          case L3_AtFaceCenter(faceDim) if faceDim == dim => stencilNodeIntegralLinear(level)
          case L3_AtFaceCenter(_)                         => stencilCellIntegralLinear(level)
        }
      }
    })

    val composed = stencils.reduceLeft(L3_StencilOps.kron)

    composed.name = name
    composed.level = level

    composed
  }
}
