package exastencils.grid

import exastencils.base.ir._
import exastencils.config.Knowledge

/// GridGeometry_staggered

trait GridGeometry_staggered extends GridGeometry {
  // additional information introduced by the staggered property
  def stagCVWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression // depends on uniform property

  // compound accesses
  def staggeredCellVolume(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], stagDim : Int) = {
    var exp : IR_Expression =
      if (0 == stagDim)
        stagCVWidth(level, index, arrayIndex, 0)
      else
        cellWidth(level, index, arrayIndex, 0)

    for (dim <- 1 until Knowledge.dimensionality)
      if (dim == stagDim)
        exp *= stagCVWidth(level, index, arrayIndex, dim)
      else
        exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

  def xStagCellVolume(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = staggeredCellVolume(level, index, arrayIndex, 0)
  def yStagCellVolume(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = staggeredCellVolume(level, index, arrayIndex, 1)
  def zStagCellVolume(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = staggeredCellVolume(level, index, arrayIndex, 2)
}
