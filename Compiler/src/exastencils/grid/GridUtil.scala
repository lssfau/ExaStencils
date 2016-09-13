package exastencils.grid

import exastencils.base.ir.IR_Expression
import exastencils.core._
import exastencils.datastructures.ir._

object GridUtil {
  // helper functions of shifting indices and accesses
  def offsetIndex(index : MultiIndex, offset : IR_Expression, dim : Int) : MultiIndex = {
    var modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }

  def offsetAccess(fieldAccess : FieldAccess, offset : IR_Expression, dim : Int) : FieldAccess = {
    var modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  // helper functions of projecting indices and accesses
  def projectIdx(baseIndex : MultiIndex, dim : Int) = {
    var index = new MultiIndex(baseIndex, new MultiIndex(Array.fill(4) { 0 }), _ * _) // keeps null entries, sets non-null entries to 0
    index(dim) = baseIndex(dim)
    index
  }
}
