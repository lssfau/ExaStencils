package exastencils.operator.l3

import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l4.L4_ActiveSlot
import exastencils.knowledge.l3.L3_KnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.stencil.l4._

/// L3_StencilTemplateAccess

object L3_StencilTemplateAccess {
  def apply(stencilName : String, level : Int) =
    new L3_StencilTemplateAccess(L3_StencilTemplateCollection.getByIdentifier(stencilName, level).get)
}

case class L3_StencilTemplateAccess(var target : L3_StencilTemplate) extends L3_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L4_StencilFieldAccess(target.name, target.level, L4_ActiveSlot, None, None, None)
}

/// L3_ResolveStencilTemplateAccesses

object L3_ResolveStencilTemplateAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_StencilTemplateCollection.exists(access.name) =>
      L3_StencilTemplateAccess(access.name, access.level.get.resolveLevel)
  })
}

