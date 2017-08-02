package exastencils.grid.l3

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.core._
import exastencils.field.l3.L3_FieldAccess

object L3_GridUtil {
  /// helper functions for shifting indices and accesses

  def offsetIndex(index : L3_ConstIndex, offset : Int, dim : Int) : L3_ConstIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetIndex(index : L3_ExpressionIndex, offset : L3_Expression, dim : Int) : L3_ExpressionIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : L3_FieldAccess, offset : Int, dim : Int) : L3_FieldAccess = {
    val modAccess = Duplicate(fieldAccess)
    if (modAccess.offset.isEmpty)
      modAccess.offset = Some(L3_ConstIndex(Array.fill(fieldAccess.target.numDimsGrid)(0)))
    modAccess.offset.get(dim) += offset
    modAccess
  }

  // helper functions for projecting indices and accesses

  def projectIdx(index : L3_ExpressionIndex, dim : Int) = {
    val modIndex = Duplicate(index)
    for (i <- 0 until modIndex.length; if dim != i)
      modIndex(dim) *= 0 // keeps null entries, sets non-null entries to 0
    modIndex
  }
}
