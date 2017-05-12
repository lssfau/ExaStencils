package exastencils.operator.l4

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_KnowledgeAccess
import exastencils.logger.Logger
import exastencils.operator.ir.IR_OffsetAccesses
import exastencils.prettyprinting.PpStream
import exastencils.stencil.ir.IR_StencilAccess

/// L4_StencilAccess

object L4_StencilAccess {
  def apply(stencilName : String, level : Int, arrayIndex : Option[Int], offset : Option[L4_ExpressionIndex], dirAccess : Option[L4_ExpressionIndex]) =
    new L4_StencilAccess(L4_StencilCollection.getByIdentifier(stencilName, level).get, arrayIndex, offset, dirAccess)
}

case class L4_StencilAccess(
    var target : L4_Stencil,
    var arrayIndex : Option[Int] = None,
    var offset : Option[L4_ExpressionIndex] = None,
    var dirAccess : Option[L4_ExpressionIndex] = None) extends L4_KnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << "@" << offset.get
    if (dirAccess.isDefined) out << ":" << dirAccess.get
  }

  def progress : IR_Expression = {
    // TODO: implement strategy converting accesses with arrayIndex or dirAccess

    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencil ${ target.name } on level ${ target.level } has dirAccess and array subscript modifiers; array index will be given precedence, dirAccess will be ignored")

    val stencil = target.getProgressedObject()

    if (arrayIndex.isDefined) {
      val coeff = Duplicate(stencil.entries(arrayIndex.get).coefficient)
      if (offset.isDefined) {
        IR_OffsetAccesses.offset = offset.get.progress
        IR_OffsetAccesses.applyStandalone(IR_ExpressionStatement(coeff))
      }
      coeff
    } else if (dirAccess.isDefined) {
      val coeff = Duplicate(stencil.findStencilEntry(dirAccess.get.progress).get.coefficient)
      if (offset.isDefined) {
        IR_OffsetAccesses.offset = offset.get.progress
        IR_OffsetAccesses.applyStandalone(IR_ExpressionStatement(coeff))
      }
      coeff
    } else {
      IR_StencilAccess(stencil, L4_ProgressOption(offset)(_.progress))
    }
  }
}

/// L4_ResolveStencilAccesses

object L4_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_StencilCollection.exists(access.name) =>
      if (access.slot.isDefined) Logger.warn("Discarding meaningless slot access on stencil")
      //if (access.offset.isDefined) Logger.warn("Discarding meaningless offset access on stencil - was a direction access (:) intended?")
      L4_StencilAccess(access.name, access.level.get.resolveLevel,
        access.arrayIndex, access.offset, access.dirAccess)
  })
}

/// L4_UnresolveStencilAccesses

object L4_UnresolveStencilAccesses extends DefaultStrategy("Revert stencil accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_StencilAccess(target, arrayIndex, offset, dirAccess) =>
      L4_UnresolvedAccess(target.name, None, Some(L4_SingleLevel(target.level)), offset, arrayIndex, dirAccess)
  })
}