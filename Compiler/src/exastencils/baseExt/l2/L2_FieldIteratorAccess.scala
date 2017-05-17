package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.baseExt.l3.L3_FieldIteratorAccess

/// L2_FieldIteratorAccess

object L2_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new L2_FieldIteratorAccess()
    ret.dim = dim
    ret
  }
}

class L2_FieldIteratorAccess() extends L2_VariableAccess("i", L2_IntegerDatatype) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def progress : L3_FieldIteratorAccess = L3_FieldIteratorAccess(dim)
}
