package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.baseExt.l2.L2_FieldIteratorAccess
import exastencils.grid.l2._

object L2_DefaultRestriction {
  def stencilNodeLinear(level : Int) : L2_Stencil = {
    def it = L2_FieldIteratorAccess(0)
    def itAsIndex = L2_ExpressionIndex(it)
    L2_Stencil("linearNode", level, 1, Array(2.0), ListBuffer(
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it - 1.0), 0.25),
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 0.0), 0.5),
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 1.0), 0.25)))
  }

  def stencilCellLinear(level : Int) : L2_Stencil = {
    def it = L2_FieldIteratorAccess(0)
    def itAsIndex = L2_ExpressionIndex(it)
    L2_Stencil("linearCell", level, 1, Array(2.0), ListBuffer(
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 0.0), 0.5),
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 1.0), 0.5)))
  }

  def stencilNodeIntegralLinear(level : Int) : L2_Stencil = {
    def it = L2_FieldIteratorAccess(0)
    def itAsIndex = L2_ExpressionIndex(it)
    L2_Stencil("integralLinearNode", level, 1, Array(2.0), ListBuffer(
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it - 1.0), 0.5),
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 0.0), 1.0),
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 1.0), 0.5)))
  }

  def stencilCellIntegralLinear(level : Int) : L2_Stencil = {
    def it = L2_FieldIteratorAccess(0)
    def itAsIndex = L2_ExpressionIndex(it)
    L2_Stencil("integralLinearCell", level, 1, Array(2.0), ListBuffer(
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 0.0), 1.0),
      L2_StencilMappingEntry(itAsIndex, L2_ExpressionIndex(2.0 * it + 1.0), 1.0)))
  }

  def generate(name : String, level : Int, numDims : Int, localization : L2_Localization, interpolation : String) : L2_Stencil = {
    val stencils = (0 until numDims).map(dim => {
      interpolation match {
        case "linear" => localization match {
          case L2_AtNode                                  => stencilNodeLinear(level)
          case L2_AtCellCenter                            => stencilCellLinear(level)
          case L2_AtFaceCenter(faceDim) if faceDim == dim => stencilNodeLinear(level)
          case L2_AtFaceCenter(_)                         => stencilCellLinear(level)
        }

        case "integral_linear" => localization match {
          case L2_AtNode                                  => stencilNodeIntegralLinear(level)
          case L2_AtCellCenter                            => stencilCellIntegralLinear(level)
          case L2_AtFaceCenter(faceDim) if faceDim == dim => stencilNodeIntegralLinear(level)
          case L2_AtFaceCenter(_)                         => stencilCellIntegralLinear(level)
        }
      }
    })

    val composed = stencils.reduceLeft(L2_StencilOps.kron)

    composed.name = name
    composed.level = level

    composed
  }
}
