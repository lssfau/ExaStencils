package exastencils.grid.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core._
import exastencils.field.ir.IR_FieldAccess

object IR_GridUtil {
  /// helper functions for shifting indices and accesses

  def offsetIndex(index : IR_ConstIndex, offset : Int, dim : Int) : IR_ConstIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetIndex(index : IR_ExpressionIndex, offset : IR_Expression, dim : Int) : IR_ExpressionIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : IR_FieldAccess, offset : Int, dim : Int) : IR_FieldAccess = {
    val modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  // helper functions for projecting indices and accesses

  def projectIdx(index : IR_ExpressionIndex, dim : Int) = {
    val modIndex = Duplicate(index)
    for (i <- 0 until modIndex.length; if dim != i)
      modIndex(dim) *= 0 // keeps null entries, sets non-null entries to 0
    modIndex
  }
}
