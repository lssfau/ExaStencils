package exastencils.field.l1

import exastencils.datastructures._
import exastencils.field.l2.L2_FieldAccess
import exastencils.knowledge.l1._
import exastencils.prettyprinting.PpStream

/// L1_FieldAccess

object L1_FieldAccess {
  def apply(access : L1_FutureFieldAccess) =
    new L1_FieldAccess(L1_FieldCollection.getByIdentifier(access.name, access.level).get)
}

case class L1_FieldAccess(var target : L1_Field) extends L1_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  override def progress = L2_FieldAccess(target.getProgressedObj(), None)
}

/// L1_ResolveFieldAccesses

object L1_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L1_FutureFieldAccess if L1_FieldCollection.exists(access.name, access.level) =>
      access.toFieldAccess
  })
}
