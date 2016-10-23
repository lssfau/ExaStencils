package exastencils.operator.l3

import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_KnowledgeAccess
import exastencils.operator.l4.L4_StencilAccess
import exastencils.prettyprinting.PpStream

/// L3_StencilAccess

object L3_StencilAccess {
  def apply(stencilName : String, level : Int) =
    new L3_StencilAccess(L3_StencilCollection.getByIdentifier(stencilName, level).get)
}

case class L3_StencilAccess(var target : L3_Stencil) extends L3_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L4_StencilAccess(target.getProgressedObject())
}

/// L3_ResolveStencilAccesses

object L3_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_StencilCollection.exists(access.name) =>
      L3_StencilAccess(access.name, access.level.get.resolveLevel)
  })
}

