package exastencils.operator.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.operator.l3.L3_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L2_StencilFieldAccess

object L2_StencilFieldAccess {
  def apply(access : L2_FutureStencilFieldAccess) =
    new L2_StencilFieldAccess(L2_StencilFieldCollection.getByIdentifier(access.name, access.level).get, access.offset, access.dirAccess)
}

case class L2_StencilFieldAccess(
    var target : L2_StencilField,
    var offset : Option[L2_ConstIndex] = None,
    var dirAccess : Option[L2_ConstIndex] = None) extends L2_LeveledKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    L3_StencilFieldAccess(target.getProgressedObj(),
      L2_ProgressOption(offset)(_.progress),
      L2_ProgressOption(dirAccess)(_.progress))
  }
}

/// L2_ResolveStencilFieldAccesses

object L2_ResolveStencilFieldAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureStencilFieldAccess if L2_StencilFieldCollection.exists(access.name, access.level) =>
      access.toStencilFieldAccess
  })
}
