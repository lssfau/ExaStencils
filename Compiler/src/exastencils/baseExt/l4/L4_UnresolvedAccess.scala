package exastencils.baseExt.l4

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.field.l4.L4_SlotSpecification
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_UnresolvedAccess

case class L4_UnresolvedAccess(
    var name : String,
    var slot : Option[L4_SlotSpecification],
    var level : Option[L4_AccessLevelSpecification],
    var offset : Option[L4_ExpressionIndex],
    var arrayIndex : Option[Int],
    var dirAccess : Option[L4_ExpressionIndex]) extends L4_Access {
  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess
  }

  def progress : IR_Expression = {
    Logger.warn(s"Progressing UnresolvedAccess $name")

    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on basic or leveled access")
    if (offset.isDefined) Logger.warn("Discarding meaningless offset access on basic or leveled access")
    if (arrayIndex.isDefined) Logger.warn("Discarding meaningless array index access on basic or leveled access")
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on basic or leveled access " + name)

    if (level.isEmpty)
      IR_StringLiteral(name)
    else
      IR_StringLiteral(name + "_" + level.get.resolveLevel)
  }
}