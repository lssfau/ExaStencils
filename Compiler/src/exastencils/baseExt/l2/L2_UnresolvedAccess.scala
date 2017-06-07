package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.field.l2.L2_SlotSpecification
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_UnresolvedAccess

object L2_UnresolvedAccess {
  def apply(name : String) = new L2_UnresolvedAccess(name, None, None, None, None, None)
  def apply(name : String, level : Option[L2_AccessLevelSpecification]) = new L2_UnresolvedAccess(name, level, None, None, None, None)
}

case class L2_UnresolvedAccess(
    var name : String,
    var level : Option[L2_AccessLevelSpecification],
    var slot : Option[L2_SlotSpecification],
    var offset : Option[L2_ExpressionIndex],
    var dirAccess : Option[L2_ExpressionIndex],
    var arrayIndex : Option[Int]) extends L2_Access {

  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  def progress = {
    Logger.warn(s"Progressing unresolved access on L3: $name" + (if (level.isDefined) s"@$level" else ""))

    L3_UnresolvedAccess(
      name,
      L2_ProgressOption(level)(_.progress),
      L2_ProgressOption(slot)(_.progress),
      L2_ProgressOption(offset)(_.progress),
      L2_ProgressOption(dirAccess)(_.progress),
      arrayIndex)
  }
}
