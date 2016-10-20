package exastencils.stencil.l4

import exastencils.base.ir.IR_Expression
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_KnowledgeAccess
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.prettyprinting.PpStream
import exastencils.stencil.ir.IR_StencilAccess

/// L4_StencilAccess

object L4_StencilAccess {
  def apply(stencilName : String, level : Int, arrayIndex : Option[Int], dirAccess : Option[L4_ExpressionIndex]) =
    new L4_StencilAccess(L4_StencilCollection.getByIdentifier(stencilName, level).get, arrayIndex, dirAccess)
}

case class L4_StencilAccess(
    var target : L4_Stencil,
    var arrayIndex : Option[Int] = None,
    var dirAccess : Option[L4_ExpressionIndex] = None) extends L4_KnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (dirAccess.isDefined) out << ":" << dirAccess
  }

  def progress : IR_Expression = {
    // TODO: implement strategy converting accesses with arrayIndex or dirAccess

    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencil ${ target.name } on level ${ target.level } has dirAccess and array subscript modifiers; array index will be given precedence, dirAccess will be ignored")

    val stencil = target.getProgressedObject()

    if (arrayIndex.isDefined)
      stencil.entries(arrayIndex.get).coefficient
    else if (dirAccess.isDefined)
      stencil.findStencilEntry(dirAccess.get.progress).get.coefficient
    else
      IR_StencilAccess(stencil)
  }
}

/// L4_ResolveStencilAccesses

object L4_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_StencilCollection.exists(access.name) =>
      if (access.slot.isDefined) Logger.warn("Discarding meaningless slot access on stencil")
      if (access.offset.isDefined) Logger.warn("Discarding meaningless offset access on stencil - was a direction access (:) intended?")
      L4_StencilAccess(access.name, access.level.get.resolveLevel,
        access.arrayIndex, access.dirAccess)
  })
}
