package exastencils.operator.l3

import exastencils.datastructures._
import exastencils.field.l4.L4_ActiveSlot
import exastencils.operator.l4.L4_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L3_StencilFieldAccess

object L3_StencilFieldAccess {
  def apply(name : String, level : Int) =
    new L3_StencilFieldAccess(L3_StencilFieldCollection.getByIdentifier(name, level).get)

  def apply(access : L3_FutureStencilFieldAccess) =
    new L3_StencilFieldAccess(L3_StencilFieldCollection.getByIdentifier(access.name, access.level).get)
}

case class L3_StencilFieldAccess(var target : L3_StencilField) extends L3_OperatorAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L4_StencilFieldAccess(target.getProgressedObj(), L4_ActiveSlot)
  override def assembleOffsetMap() = target.stencil.assembleOffsetMap()
}

/// L3_ResolveStencilFieldAccesses

object L3_ResolveStencilFieldAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureStencilFieldAccess if L3_StencilFieldCollection.exists(access.name, access.level) =>
      access.toStencilFieldAccess
  })
}
