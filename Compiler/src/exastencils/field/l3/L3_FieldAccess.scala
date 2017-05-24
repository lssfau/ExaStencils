package exastencils.field.l3

import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge.l3._
import exastencils.prettyprinting.PpStream

/// L3_FieldAccess

object L3_FieldAccess {
  def apply(name : String, level : Int) =
    new L3_FieldAccess(L3_FieldCollection.getByIdentifier(name, level).get)

  def apply(access : L3_FutureFieldAccess) =
    new L3_FieldAccess(L3_FieldCollection.getByIdentifier(access.name, access.level).get)
}

case class L3_FieldAccess(var target : L3_Field) extends L3_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  override def progress = L4_FieldAccess(target.getProgressedObj(), L4_ActiveSlot)
}

/// L3_ResolveFieldAccesses

object L3_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureFieldAccess if L3_FieldCollection.exists(access.name, access.level) =>
      access.toFieldAccess
  })
}
