package exastencils.grid.l4

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.core._
import exastencils.field.l4.L4_FieldAccess

object L4_GridUtil {
  /// helper functions for shifting indices and accesses

  def offsetIndex(index : L4_ConstIndex, offset : Int, dim : Int) : L4_ConstIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetIndex(index : L4_ExpressionIndex, offset : L4_Expression, dim : Int) : L4_ExpressionIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : L4_FieldAccess, offset : Int, dim : Int) : L4_FieldAccess = {
    val modAccess = Duplicate(fieldAccess)
    if (modAccess.offset.isEmpty)
      modAccess.offset = Some(L4_ConstIndex(Array.fill(fieldAccess.target.numDimsGrid)(0)))
    modAccess.offset.get(dim) += offset
    modAccess
  }

  /// helper functions for projecting indices and accesses

  def projectIdx(index : L4_ExpressionIndex, dim : Int) = {
    val modIndex = Duplicate(index)
    for (i <- 0 until modIndex.length; if dim != i)
      modIndex(i) *= 0 // keeps null entries, sets non-null entries to 0
    modIndex
  }
}
