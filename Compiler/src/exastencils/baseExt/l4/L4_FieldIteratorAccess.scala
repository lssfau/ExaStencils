package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_FieldIteratorAccess

/// L4_FieldIteratorAccess

object L4_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new L4_FieldIteratorAccess()
    ret.dim = dim
    ret
  }
}

class L4_FieldIteratorAccess() extends L4_VariableAccess("i", L4_IntegerDatatype) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def progress : IR_FieldIteratorAccess = IR_FieldIteratorAccess(dim)
}
