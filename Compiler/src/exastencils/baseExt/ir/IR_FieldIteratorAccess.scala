package exastencils.baseExt.ir

import exastencils.base.ir._

/// IR_FieldIteratorAccess

object IR_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new IR_FieldIteratorAccess()
    ret.dim = dim
    ret
  }
}

class IR_FieldIteratorAccess() extends IR_VariableAccess("i", IR_IntegerDatatype) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_
}
