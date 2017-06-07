package exastencils.operator.l4

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4._
import exastencils.datastructures._
import exastencils.deprecated.ir._
import exastencils.field.l4._
import exastencils.logger.Logger
import exastencils.operator.ir.IR_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L4_StencilFieldAccess

object L4_StencilFieldAccess {
  def apply(access : L4_FutureStencilFieldAccess) =
    new L4_StencilFieldAccess(L4_StencilFieldCollection.getByIdentifier(access.name, access.level).get, access.slot, access.arrayIndex, access.offset, access.dirAccess)

  def apply(stencilFieldName : String, level : Int, slot : L4_SlotSpecification, arrayIndex : Option[Int], offset : Option[L4_ExpressionIndex], dirAccess : Option[L4_ExpressionIndex])
  = new L4_StencilFieldAccess(L4_StencilFieldCollection.getByIdentifier(stencilFieldName, level).get, slot, arrayIndex, offset, dirAccess)
}

case class L4_StencilFieldAccess(
    var target : L4_StencilField,
    var slot : L4_SlotSpecification,
    var arrayIndex : Option[Int] = None,
    var offset : Option[L4_ExpressionIndex] = None,
    var dirAccess : Option[L4_ExpressionIndex] = None) extends L4_OperatorAccess {

  override def prettyprint(out : PpStream) = {
    out << target.name
    if (target.field.numSlots > 1) out << '[' << slot << ']'
    out << '@' << target.level
    if (offset.isDefined) out << "@" << offset
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ":" << dirAccess
  }

  def progressOffset(numDims : Int) = {
    if (offset.isDefined) {
      val progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      Some(progressedOffset)
    } else {
      None
    }
  }

  def progress : IR_StencilFieldAccess = {
    if (arrayIndex.isDefined) Logger.warn("Unresolved arrayIndex")
    if (dirAccess.isDefined) Logger.warn("Unresolved dirAccess")

    val numDims = target.field.fieldLayout.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)

    IR_StencilFieldAccess(IR_StencilFieldSelection(target.getProgressedObj(), target.field.level, L4_FieldAccess.resolveSlot(target.field.getProgressedObj(), slot)),
      index, progressOffset(index.length))
  }

  override def assembleOffsetMap() = target.stencil.assembleOffsetMap()
}

/// L4_ResolveStencilFieldAccesses

object L4_ResolveStencilFieldAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureStencilFieldAccess if L4_StencilFieldCollection.exists(access.name, access.level) =>
      access.toStencilFieldAccess
  })
}

/// L4_ResolveStencilFieldComponentAccesses

object L4_ResolveStencilFieldComponentAccesses extends DefaultStrategy("Resolve accesses to single components of stencil fields") {
  this += new Transformation("Resolve applicable accesses", {
    case access : L4_StencilFieldAccess if access.arrayIndex.isDefined =>
      if (access.dirAccess.isDefined)
        Logger.warn(s"Access to stencil field ${ access.target.name } on level ${ access.target.level } has dirAccess and array subscript modifiers; " +
          "array index will be given precedence, dirAccess will be ignored")

      L4_FieldAccess(access.target.field, access.slot, access.arrayIndex, access.offset)

    case access : L4_StencilFieldAccess if access.dirAccess.isDefined =>
      L4_FieldAccess(access.target.field, access.slot, access.target.stencil.findStencilEntryIndex(access.dirAccess.get.toConstIndex), access.offset)
  })
}

/// L4_UnresolveStencilFieldAccesses

object L4_UnresolveStencilFieldAccesses extends DefaultStrategy("Revert stencil field accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_StencilFieldAccess(target, slot, arrayIndex, offset, dirAccess) =>
      val newSlot = if (L4_ActiveSlot == slot) None else Some(slot)
      L4_UnresolvedAccess(target.name, Some(L4_SingleLevel(target.level)), newSlot, offset, dirAccess, arrayIndex)
  })
}
