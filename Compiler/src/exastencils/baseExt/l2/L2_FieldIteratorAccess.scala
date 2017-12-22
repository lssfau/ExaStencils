package exastencils.baseExt.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l3.L3_FieldIteratorAccess
import exastencils.logger.Logger

/// L2_FieldIteratorAccess

object L2_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new L2_FieldIteratorAccess()
    ret.dim = dim
    ret
  }

  def apply(ident : String) : L2_FieldIteratorAccess = {
    if (ident.startsWith("i") && ident.substring(1).forall(_.isDigit)) {
      this (ident.substring(1).toInt)
    } else if (List("x", "y", "z").contains(ident.toLowerCase())) {
      Logger.warn(s"Using $ident as iterator access is deprecated; please switch to i${ ident.toLowerCase().head.toInt - 'x'.toInt }")
      this (ident.toLowerCase().head.toInt - 'x'.toInt)
    } else {
      Logger.error(s"Invalid identifier in L2 FieldIteratorAccess: $ident")
    }
  }

  def fullIndex(numDims : Int) = L2_ExpressionIndex((0 until numDims).map(this (_) : L2_Expression).toArray)
}

class L2_FieldIteratorAccess() extends L2_PlainVariableAccess("i", L2_IntegerDatatype, false) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def progress = ProgressLocation(L3_FieldIteratorAccess(dim))

  override def equals(obj : scala.Any) = {
    obj match {
      case other : L2_FieldIteratorAccess => other.dim == dim
      case other : L2_VariableAccess      => other.name == name
      case _                              => super.equals(obj)
    }
  }
}
