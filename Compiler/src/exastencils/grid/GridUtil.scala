package exastencils.grid

import exastencils.base.ir._
import exastencils.core._
import exastencils.field.ir.IR_FieldAccess

@deprecated("Switch to IR_GridUtil instead", "02.08.17")
object GridUtil {
  // helper functions of shifting indices and accesses
  def offsetIndex(index : IR_ExpressionIndex, offset : IR_Expression, dim : Int) : IR_ExpressionIndex = {
    val modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : IR_FieldAccess, offset : IR_Expression, dim : Int) : IR_FieldAccess = {
    val modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  // helper functions of projecting indices and accesses
  def projectIdx(baseIndex : IR_ExpressionIndex, dim : Int) = {
    val index = IR_ExpressionIndex(baseIndex, IR_ExpressionIndex(Array.fill(baseIndex.length) { 0 }), _ * _) // keeps null entries, sets non-null entries to 0
    index(dim) = baseIndex(dim)
    index
  }
}
