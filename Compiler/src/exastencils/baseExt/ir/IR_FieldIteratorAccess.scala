package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

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

  override def prettyprint(out : PpStream) : Unit = /* FIXME: remove this override and use actual identifier */ out << ('x'.toInt + dim).toChar

  override def equals(obj : scala.Any) = {
    obj match {
      case other : IR_FieldIteratorAccess => other.dim == dim
      case other : IR_VariableAccess      => other.name == name
      case _                              => super.equals(obj)
    }
  }
}
