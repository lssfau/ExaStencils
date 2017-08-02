package exastencils.baseExt.l3

import exastencils.base.l3._
import exastencils.baseExt.l4.L4_FieldIteratorAccess
import exastencils.logger.Logger

/// L3_FieldIteratorAccess

object L3_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new L3_FieldIteratorAccess()
    ret.dim = dim
    ret
  }

  def apply(ident : String) : L3_FieldIteratorAccess = {
    if (ident.startsWith("i") && ident.substring(1).forall(_.isDigit)) {
      this (ident.substring(1).toInt)
    } else if (List("x", "y", "z").contains(ident.toLowerCase())) {
      Logger.warn(s"Using $ident as iterator access is deprecated; please switch to i${ ident.toLowerCase().head.toInt - 'x'.toInt }")
      this (ident.toLowerCase().head.toInt - 'x'.toInt)
    } else {
      Logger.error(s"Invalid identifier in L3 FieldIteratorAccess: $ident")
    }
  }

  def fullIndex(numDims : Int) = L3_ExpressionIndex((0 until numDims).map(this (_) : L3_Expression).toArray)
}

class L3_FieldIteratorAccess() extends L3_PlainVariableAccess("i", L3_IntegerDatatype, false) {
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
