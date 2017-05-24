package exastencils.stencil.l4

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.deprecated.ir._
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.l4._
import exastencils.knowledge.l4.L4_KnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.stencil.ir.IR_StencilFieldAccess

/// L4_StencilFieldAccess

object L4_StencilFieldAccess {
  def apply(stencilFieldName : String, level : Int, slot : L4_SlotSpecification, arrayIndex : Option[Int], offset : Option[L4_ExpressionIndex], dirAccess : Option[L4_ExpressionIndex])
  = new L4_StencilFieldAccess(L4_StencilFieldCollection.getByIdentifier(stencilFieldName, level).get, slot, arrayIndex, offset, dirAccess)
}

case class L4_StencilFieldAccess(
    var target : L4_StencilField,
    var slot : L4_SlotSpecification,
    var arrayIndex : Option[Int] = None,
    var offset : Option[L4_ExpressionIndex] = None,
    var dirAccess : Option[L4_ExpressionIndex] = None) extends L4_KnowledgeAccess {

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

  def progress : IR_Expression = {
    // TODO: extract mapping to FieldAccess for cases where single entries are targeted into a separate strategy

    val stencilField = target.getProgressedObj()
    val field = stencilField.field
    val numDims = stencilField.field.fieldLayout.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)

    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencil field ${ target.name } on level ${ target.level } has direction access and array subscript modifiers; array index will be given precedence, offset will be ignored")

    if (arrayIndex.isDefined) {
      index.indices :+= IR_IntegerConstant(arrayIndex.get)
      IR_FieldAccess(IR_FieldSelection(field, field.level, L4_FieldAccess.resolveSlot(field, slot)), index, progressOffset(index.length))
    } else if (dirAccess.isDefined) {
      index.indices :+= IR_IntegerConstant(stencilField.findOffsetIndex(dirAccess.get.progress).get)
      IR_FieldAccess(IR_FieldSelection(field, field.level, L4_FieldAccess.resolveSlot(field, slot)), index, progressOffset(index.length))
    } else {
      IR_StencilFieldAccess(IR_StencilFieldSelection(stencilField, stencilField.field.level, L4_FieldAccess.resolveSlot(stencilField.field, slot)),
        index, progressOffset(index.length))
    }
  }
}

/// L4_ResolveStencilFieldAccesses

object L4_ResolveStencilFieldAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_StencilFieldCollection.exists(access.name) =>
      L4_StencilFieldAccess(access.name, access.level.get.resolveLevel,
        access.slot.getOrElse(L4_ActiveSlot), access.arrayIndex, access.offset, access.dirAccess)
  })
}

/// L4_UnresolveStencilFieldAccesses

object L4_UnresolveStencilFieldAccesses extends DefaultStrategy("Revert stencil field accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_StencilFieldAccess(target, slot, arrayIndex, offset, dirAccess) =>
      val newSlot = if (L4_ActiveSlot == slot) None else Some(slot)
      L4_UnresolvedAccess(target.name, newSlot, Some(L4_SingleLevel(target.level)), offset, arrayIndex, dirAccess)
  })
}
