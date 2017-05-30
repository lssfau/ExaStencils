package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_FieldIteratorAccess

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

  def generate(name : String, level : Int, numDims : Int, localization : String, interpolation : String) : L4_Stencil = {
    val stencils = (0 until numDims).map(dim => {
      interpolation match {
        case "linear" => localization.toLowerCase() match {
          case "node"               => stencilNodeLinear(level)
          case "cell"               => stencilCellLinear(level)
          case "face_x" if 0 == dim => stencilNodeLinear(level)
          case "face_x"             => stencilCellLinear(level)
          case "face_y" if 1 == dim => stencilNodeLinear(level)
          case "face_y"             => stencilCellLinear(level)
          case "face_z" if 2 == dim => stencilNodeLinear(level)
          case "face_z"             => stencilCellLinear(level)
        }

        case "integral_linear" => localization.toLowerCase() match {
          case "node"               => stencilNodeIntegralLinear(level)
          case "cell"               => stencilCellIntegralLinear(level)
          case "face_x" if 0 == dim => stencilNodeIntegralLinear(level)
          case "face_x"             => stencilCellIntegralLinear(level)
          case "face_y" if 1 == dim => stencilNodeIntegralLinear(level)
          case "face_y"             => stencilCellIntegralLinear(level)
          case "face_z" if 2 == dim => stencilNodeIntegralLinear(level)
          case "face_z"             => stencilCellIntegralLinear(level)
        }
      }
    })

    val composed = stencils.reduceLeft(L4_StencilOps.kron)

    composed.name = name
    composed.level = level

    composed
  }
}
