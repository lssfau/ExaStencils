package exastencils.field.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge.l3._
import exastencils.prettyprinting.PpStream

/// L3_FieldAccess

object L3_FieldAccess {
  def apply(access : L3_FutureFieldAccess) =
    new L3_FieldAccess(L3_FieldCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L3_FieldAccess(
    var target : L3_Field,
    var offset : Option[L3_ConstIndex] = None) extends L3_LeveledKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = {
    L4_FieldAccess(target.getProgressedObj(),
      L4_ActiveSlot,
      L3_ProgressOption(offset)(_.progress))
  }
}

/// L3_ResolveFieldAccesses

object L3_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureFieldAccess if L3_FieldCollection.exists(access.name, access.level) =>
      access.toFieldAccess
  })
}
