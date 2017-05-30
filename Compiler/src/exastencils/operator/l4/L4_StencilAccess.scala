package exastencils.operator.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.operator.ir.IR_StencilAccess
import exastencils.prettyprinting.PpStream

/// L4_StencilAccess

object L4_StencilAccess {
  def apply(access : L4_FutureStencilAccess) =
    new L4_StencilAccess(L4_StencilCollection.getByIdentifier(access.name, access.level).get, access.arrayIndex, access.offset, access.dirAccess)

  def apply(name : String, level : Int, arrayIndex : Option[Int], offset : Option[L4_ExpressionIndex], dirAccess : Option[L4_ExpressionIndex]) =
    new L4_StencilAccess(L4_StencilCollection.getByIdentifier(name, level).get, arrayIndex, offset, dirAccess)
}

case class L4_StencilAccess(
    var target : L4_Stencil,
    var arrayIndex : Option[Int] = None,
    var offset : Option[L4_ExpressionIndex] = None,
    var dirAccess : Option[L4_ExpressionIndex] = None) extends L4_OperatorAccess {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << "@" << offset.get
    if (dirAccess.isDefined) out << ":" << dirAccess.get
  }

  def progress : IR_StencilAccess = {
    if (arrayIndex.isDefined) Logger.warn("Unresolved arrayIndex")
    if (dirAccess.isDefined) Logger.warn("Unresolved dirAccess")

    IR_StencilAccess(target.getProgressedObj(), L4_ProgressOption(offset)(_.progress))
  }

  override def assembleOffsetMap() = target.assembleOffsetMap()
}

/// L4_ResolveStencilComponentAccesses

object L4_ResolveStencilComponentAccesses extends DefaultStrategy("Resolve accesses to single components of stencils") {
  this += new Transformation("Resolve applicable accesses", {
    case access : L4_StencilAccess if access.arrayIndex.isDefined =>
      if (access.dirAccess.isDefined)
        Logger.warn(s"Access to stencil ${ access.target.name } on level ${ access.target.level } has dirAccess and array subscript modifiers; " +
          "array index will be given precedence, dirAccess will be ignored")

      val coeff = L4_ExpressionStatement(Duplicate(access.target.entries(access.arrayIndex.get).coefficient))
      if (access.offset.isDefined) {
        L4_OffsetAccesses.offset = access.offset.get
        L4_OffsetAccesses.applyStandalone(coeff)
      }
      coeff.expression

    case access : L4_StencilAccess if access.dirAccess.isDefined =>
      val coeff = L4_ExpressionStatement(Duplicate(access.target.findStencilEntry(access.dirAccess.get.toConstIndex).get.coefficient))
      if (access.offset.isDefined) {
        L4_OffsetAccesses.offset = access.offset.get
        L4_OffsetAccesses.applyStandalone(coeff)
      }
      coeff.expression
  })
}

/// L4_ResolveStencilAccesses

object L4_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureStencilAccess if L4_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}

/// L4_UnresolveStencilAccesses

object L4_UnresolveStencilAccesses extends DefaultStrategy("Revert stencil accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_StencilAccess(target, arrayIndex, offset, dirAccess) =>
      L4_UnresolvedAccess(target.name, None, Some(L4_SingleLevel(target.level)), offset, arrayIndex, dirAccess)
  })
}
