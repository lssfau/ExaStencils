package exastencils.operator.l2

import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.operator.l3.L3_StencilAccess
import exastencils.prettyprinting.PpStream

/// L2_StencilAccess

object L2_StencilAccess {
  def apply(name : String, level : Int) =
    new L2_StencilAccess(L2_StencilCollection.getByIdentifier(name, level).get)

  def apply(access : L2_FutureStencilAccess) =
    new L2_StencilAccess(L2_StencilCollection.getByIdentifier(access.name, access.level).get)
}

case class L2_StencilAccess(var target : L2_Stencil) extends L2_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L3_StencilAccess(target.getProgressedObject())
}

/// L2_ResolveStencilAccesses

object L2_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureStencilAccess if L2_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}
