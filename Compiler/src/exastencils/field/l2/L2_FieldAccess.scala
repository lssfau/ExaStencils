package exastencils.field.l2

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.field.l3.L3_FieldAccess
import exastencils.knowledge.l2._
import exastencils.prettyprinting.PpStream

/// L2_FieldAccess

object L2_FieldAccess {
  def apply(access : L2_FutureFieldAccess) =
    new L2_FieldAccess(L2_FieldCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L2_FieldAccess(
    var target : L2_Field,
    var offset : Option[L2_ConstIndex] = None) extends L2_LeveledKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def offsetWith(newOffset : L2_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  override def progress = {
    L3_FieldAccess(target.getProgressedObj(),
      L2_ProgressOption(offset)(_.progress))
  }
}

/// L2_ResolveFieldAccesses

object L2_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureFieldAccess if L2_FieldCollection.exists(access.name, access.level) =>
      access.toFieldAccess
  })
}
