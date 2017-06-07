package exastencils.operator.l2

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.operator.l3.L3_StencilAccess
import exastencils.prettyprinting.PpStream

/// L2_StencilAccess

object L2_StencilAccess {
  def apply(access : L2_FutureStencilAccess) =
    new L2_StencilAccess(L2_StencilCollection.getByIdentifier(access.name, access.level).get, access.dirAccess)
}

case class L2_StencilAccess(
    var target : L2_Stencil,
    var dirAccess : Option[L2_ExpressionIndex] = None) extends L2_OperatorAccess {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  def progress = {
    L3_StencilAccess(target.getProgressedObj(),
      L2_ProgressOption(dirAccess)(_.progress))
  }
  override def assembleOffsetMap() = target.assembleOffsetMap()
}

/// L2_ResolveStencilAccesses

object L2_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureStencilAccess if L2_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}
