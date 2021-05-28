package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_VariableAccess

/// IR_WaLBerlaFieldIteratorAccess

object IR_WaLBerlaFieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new IR_WaLBerlaFieldIteratorAccess()
    ret.dim = dim
    ret
  }

  def fullIndex(numDims : Int) = IR_ExpressionIndex((0 until numDims).map(this (_) : IR_Expression).toArray)
}

class IR_WaLBerlaFieldIteratorAccess() extends IR_VariableAccess("i0", IR_SpecialDatatype("cell_idx_t")) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def equals(obj : scala.Any) = {
    obj match {
      case other : IR_WaLBerlaFieldIteratorAccess => other.dim == dim
      case other : IR_VariableAccess              => other.name == name
      case _                                      => super.equals(obj)
    }
  }
}
