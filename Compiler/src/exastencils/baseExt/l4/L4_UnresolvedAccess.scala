package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.field.l4.L4_SlotSpecification
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_UnresolvedAccess

object L4_UnresolvedAccess {
  def apply(name : String) = new L4_UnresolvedAccess(name, None, None, None, None, None)
  def apply(name : String, level : Option[L4_AccessLevelSpecification]) = new L4_UnresolvedAccess(name, level, None, None, None, None)
}

case class L4_UnresolvedAccess(
    var name : String,
    var level : Option[L4_AccessLevelSpecification],
    var slot : Option[L4_SlotSpecification],
    var offset : Option[L4_ConstIndex],
    var dirAccess : Option[L4_ConstIndex],
    var arrayIndex : Option[Int]) extends L4_Access {

  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress : IR_Expression = ProgressLocation {
    Logger.warn(s"Progressing unresolved access on L4: $name" + (if (level.isDefined) s"@${ level.get }" else ""))

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
