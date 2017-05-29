package exastencils.field.l4

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_KnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FieldLayoutAccess

object L4_FieldLayoutAccess {
  def apply(access : L4_FutureFieldLayoutAccess) =
    new L4_FieldLayoutAccess(L4_FieldLayoutCollection.getByIdentifier(access.name, access.level).get)
}

case class L4_FieldLayoutAccess(var target : L4_FieldLayout) extends L4_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << "@" << target.level
  override def progress = Logger.error(s"Trying to progress access to field layout ${ target.name }@${ target.level } - unsupported")
}

/// L4_ResolveFieldLayoutAccesses

object L4_ResolveFieldLayoutAccesses extends DefaultStrategy("Resolve accesses to field layouts") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureFieldLayoutAccess if L4_FieldLayoutCollection.exists(access.name, access.level) =>
      access.toFieldLayoutAccess
  })
}

/// L4_UnresolveFieldLayoutAccesses

object L4_UnresolveFieldLayoutAccesses extends DefaultStrategy("Revert field layout accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_FieldLayoutAccess(target) =>
      L4_UnresolvedAccess(target.name, None, Some(L4_SingleLevel(target.level)), None, None, None)
  })
}