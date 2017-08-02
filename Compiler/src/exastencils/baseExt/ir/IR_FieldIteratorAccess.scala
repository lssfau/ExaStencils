package exastencils.baseExt.ir

import exastencils.base.ir._

/// IR_FieldIteratorAccess

object IR_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new IR_FieldIteratorAccess()
    ret.dim = dim
    ret
  }

  def fullIndex(numDims : Int) = IR_ExpressionIndex((0 until numDims).map(this (_) : IR_Expression).toArray)
}

class IR_FieldIteratorAccess() extends IR_VariableAccess("i", IR_IntegerDatatype) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def equals(obj : scala.Any) = {
    obj match {
      case other : IR_FieldIteratorAccess => other.dim == dim
      case other : IR_VariableAccess      => other.name == name
      case _                              => super.equals(obj)
    }
  }
}
