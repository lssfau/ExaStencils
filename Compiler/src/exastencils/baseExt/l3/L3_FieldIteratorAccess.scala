package exastencils.baseExt.l3

import exastencils.base.l3._
import exastencils.baseExt.l4.L4_FieldIteratorAccess

/// L3_FieldIteratorAccess

object L3_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new L3_FieldIteratorAccess()
    ret.dim = dim
    ret
  }
}

class L3_FieldIteratorAccess() extends L3_VariableAccess("i", L3_IntegerDatatype) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def progress = L4_FieldIteratorAccess(dim)

  override def equals(obj : scala.Any) = {
    obj match {
      case other : L3_FieldIteratorAccess => other.dim == dim
      case other : L3_VariableAccess      => other.name == name
      case _                              => super.equals(obj)
    }
  }
}
