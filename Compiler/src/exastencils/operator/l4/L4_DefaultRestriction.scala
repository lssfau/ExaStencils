package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_FieldIteratorAccess
import exastencils.grid.l4._

object L4_DefaultRestriction {
  def stencilNodeLinear(level : Int) : L4_Stencil = {
    def it = L4_FieldIteratorAccess(0)
    def itAsIndex = L4_ExpressionIndex(it)
    L4_Stencil("linearNode", level, 1, Array(2.0), ListBuffer(
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it - 1.0), 0.25),
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 0.0), 0.5),
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 1.0), 0.25)))
  }

  def stencilCellLinear(level : Int) : L4_Stencil = {
    def it = L4_FieldIteratorAccess(0)
    def itAsIndex = L4_ExpressionIndex(it)
    L4_Stencil("linearCell", level, 1, Array(2.0), ListBuffer(
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 0.0), 0.5),
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 1.0), 0.5)))
  }

  def stencilNodeIntegralLinear(level : Int) : L4_Stencil = {
    def it = L4_FieldIteratorAccess(0)
    def itAsIndex = L4_ExpressionIndex(it)
    L4_Stencil("integralLinearNode", level, 1, Array(2.0), ListBuffer(
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it - 1.0), 0.5),
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 0.0), 1.0),
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 1.0), 0.5)))
  }

  def stencilCellIntegralLinear(level : Int) : L4_Stencil = {
    def it = L4_FieldIteratorAccess(0)
    def itAsIndex = L4_ExpressionIndex(it)
    L4_Stencil("integralLinearCell", level, 1, Array(2.0), ListBuffer(
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 0.0), 1.0),
      L4_StencilMappingEntry(itAsIndex, L4_ExpressionIndex(2.0 * it + 1.0), 1.0)))
  }

  def generate(name : String, level : Int, numDims : Int, localization : L4_Localization, interpolation : String) : L4_Stencil = {
    val stencils = (0 until numDims).map(dim => {
      interpolation match {
        case "linear" => localization match {
          case L4_AtNode                                  => stencilNodeLinear(level)
          case L4_AtCellCenter                            => stencilCellLinear(level)
          case L4_AtFaceCenter(faceDim) if faceDim == dim => stencilNodeLinear(level)
          case L4_AtFaceCenter(_)                         => stencilCellLinear(level)
        }

        case "integral_linear" => localization match {
          case L4_AtNode                                  => stencilNodeIntegralLinear(level)
          case L4_AtCellCenter                            => stencilCellIntegralLinear(level)
          case L4_AtFaceCenter(faceDim) if faceDim == dim => stencilNodeIntegralLinear(level)
          case L4_AtFaceCenter(_)                         => stencilCellIntegralLinear(level)
        }
      }
    })

    val composed = stencils.reduceLeft(L4_StencilOps.kron)

    composed.name = name
    composed.level = level

    composed
  }
}
