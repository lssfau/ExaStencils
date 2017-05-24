package exastencils.operator.l3

import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.operator.l4.L4_StencilAccess
import exastencils.prettyprinting.PpStream

/// L3_StencilAccess

object L3_StencilAccess {
  def apply(name : String, level : Int) =
    new L3_StencilAccess(L3_StencilCollection.getByIdentifier(name, level).get)

  def apply(access : L3_FutureStencilAccess) =
    new L3_StencilAccess(L3_StencilCollection.getByIdentifier(access.name, access.level).get)
}

case class L3_StencilAccess(var target : L3_Stencil) extends L3_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L4_StencilAccess(target.getProgressedObj())
}

/// L3_ResolveStencilAccesses

object L3_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureStencilAccess if L3_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}
