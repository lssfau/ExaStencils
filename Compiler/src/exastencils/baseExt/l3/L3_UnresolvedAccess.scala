package exastencils.baseExt.l3

import exastencils.base.l3._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.field.l3.L3_SlotSpecification
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_UnresolvedAccess

object L3_UnresolvedAccess {
  def apply(name : String) = new L3_UnresolvedAccess(name, None, None, None, None, None)
  def apply(name : String, level : Option[L3_AccessLevelSpecification]) = new L3_UnresolvedAccess(name, level, None, None, None, None)
}

case class L3_UnresolvedAccess(
    var name : String,
    var level : Option[L3_AccessLevelSpecification],
    var slot : Option[L3_SlotSpecification],
    var offset : Option[L3_ConstIndex],
    var dirAccess : Option[L3_ConstIndex],
    var arrayIndex : Option[Int]) extends L3_Access {

  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  def progress = {
    Logger.warn(s"Progressing unresolved access on L3: $name" + (if (level.isDefined) s"@${ level.get }" else ""))

    L4_UnresolvedAccess(
      name,
      L3_ProgressOption(level)(_.progress),
      L3_ProgressOption(slot)(_.progress),
      L3_ProgressOption(offset)(_.progress),
      L3_ProgressOption(dirAccess)(_.progress),
      arrayIndex)
  }
}
