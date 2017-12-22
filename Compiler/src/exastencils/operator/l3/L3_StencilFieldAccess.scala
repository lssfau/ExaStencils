package exastencils.operator.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l4.L4_ActiveSlot
import exastencils.knowledge.l3._
import exastencils.operator.l4.L4_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L3_StencilFieldAccess

object L3_StencilFieldAccess {
  def apply(access : L3_FutureStencilFieldAccess) =
    new L3_StencilFieldAccess(L3_StencilFieldCollection.getByIdentifier(access.name, access.level).get, access.offset, access.dirAccess)
}

case class L3_StencilFieldAccess(
    var target : L3_StencilField,
    var offset : Option[L3_ConstIndex] = None,
    var dirAccess : Option[L3_ConstIndex] = None) extends L3_LeveledKnowledgeAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def offsetWith(newOffset : L3_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  override def progress = ProgressLocation {
    L4_StencilFieldAccess(target.getProgressedObj(),
      L4_ActiveSlot,
      L3_ProgressOption(offset)(_.progress),
      L3_ProgressOption(dirAccess)(_.progress))
  }
}

/// L3_ResolveStencilFieldAccesses

object L3_ResolveStencilFieldAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureStencilFieldAccess if L3_StencilFieldCollection.exists(access.name, access.level) =>
      access.toStencilFieldAccess
  })
}
