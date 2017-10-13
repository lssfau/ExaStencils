package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.logger.Logger

/// L4_FieldIteratorAccess

object L4_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new L4_FieldIteratorAccess()
    ret.dim = dim
    ret
  }

  def apply(ident : String) : L4_FieldIteratorAccess = {
    if (ident.startsWith("i") && ident.substring(1).forall(_.isDigit)) {
      this (ident.substring(1).toInt)
    } else if (List("x", "y", "z").contains(ident.toLowerCase())) {
      Logger.warn(s"Using $ident as iterator access is deprecated; please switch to i${ ident.toLowerCase().head.toInt - 'x'.toInt }")
      this (ident.toLowerCase().head.toInt - 'x'.toInt)
    } else {
      Logger.error(s"Invalid identifier in L4 FieldIteratorAccess: $ident")
    }
  }

  def fullIndex(numDims : Int) = L4_ExpressionIndex((0 until numDims).map(this (_) : L4_Expression).toArray)
}

class L4_FieldIteratorAccess() extends L4_PlainVariableAccess("i", L4_IntegerDatatype, false) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def progress = IR_FieldIteratorAccess(dim)

  override def equals(obj : scala.Any) = {
    obj match {
      case other : L4_FieldIteratorAccess => other.dim == dim
      case other : L4_VariableAccess      => other.name == name
      case _                              => super.equals(obj)
    }
  }
}
