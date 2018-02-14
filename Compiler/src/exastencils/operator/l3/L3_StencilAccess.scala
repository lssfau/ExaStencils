package exastencils.operator.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.operator.l4.L4_StencilAccess
import exastencils.prettyprinting.PpStream

/// L3_StencilAccess

object L3_StencilAccess {
  def apply(access : L3_FutureStencilAccess) =
    new L3_StencilAccess(L3_StencilCollection.getByIdentifier(access.name, access.level).get, access.offset, access.dirAccess)
}

case class L3_StencilAccess(
    var target : L3_Stencil,
    var offset : Option[L3_ConstIndex] = None,
    var dirAccess : Option[L3_ConstIndex] = None) extends L3_OperatorAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    L4_StencilAccess(target.getProgressedObj(),
      L3_ProgressOption(offset)(_.progress),
      L3_ProgressOption(dirAccess)(_.progress))
  }
  override def assembleOffsetMap() = target.assembleOffsetMap()
}

/// L3_ResolveStencilAccesses

object L3_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureStencilAccess if L3_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}
