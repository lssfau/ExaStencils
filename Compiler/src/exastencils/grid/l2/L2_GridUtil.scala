package exastencils.grid.l2

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.core._
import exastencils.field.l2.L2_FieldAccess

object L2_GridUtil {
  /// helper functions for shifting indices and accesses

  def offsetIndex(index : L2_ConstIndex, offset : Int, dim : Int) : L2_ConstIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetIndex(index : L2_ExpressionIndex, offset : L2_Expression, dim : Int) : L2_ExpressionIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : L2_FieldAccess, offset : Int, dim : Int) : L2_FieldAccess = {
    val modAccess = Duplicate(fieldAccess)
    if (modAccess.offset.isEmpty)
      modAccess.offset = Some(L2_ConstIndex(Array.fill(fieldAccess.target.numDimsGrid)(0)))
    modAccess.offset.get(dim) += offset
    modAccess
  }

  /// helper functions for projecting indices and accesses

  def projectIdx(index : L2_ExpressionIndex, dim : Int) = {
    val modIndex = Duplicate(index)
    for (i <- 0 until modIndex.length; if dim != i)
      modIndex(i) *= 0 // keeps null entries, sets non-null entries to 0
    modIndex
  }
}
