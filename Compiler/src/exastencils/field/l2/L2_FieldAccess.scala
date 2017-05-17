package exastencils.field.l2

import exastencils.datastructures._
import exastencils.field.l3.L3_FieldAccess
import exastencils.knowledge.l2._
import exastencils.prettyprinting.PpStream

/// L2_FieldAccess

object L2_FieldAccess {
  def apply(name : String, level : Int) =
    new L2_FieldAccess(L2_FieldCollection.getByIdentifier(name, level).get)

  def apply(access : L2_FutureFieldAccess) =
    new L2_FieldAccess(L2_FieldCollection.getByIdentifier(access.name, access.level).get)
}

case class L2_FieldAccess(var target : L2_Field) extends L2_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L3_FieldAccess(target.getProgressedObject())
}

/// L2_ResolveFieldAccesses

object L2_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureFieldAccess if L2_FieldCollection.exists(access.name, access.level) =>
      access.toFieldAccess
  })
}
