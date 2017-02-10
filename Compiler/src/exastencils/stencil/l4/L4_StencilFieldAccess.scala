package exastencils.stencil.l4

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.config._
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

  def progress : IR_Expression = {
    // TODO: extract mapping to FieldAccess for cases where single entries are targeted into a separate strategy

    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencil field ${ target.name } on level ${ target.level } has direction access and array subscript modifiers; array index will be given precedence, offset will be ignored")

    val stencilField = target.getProgressedObject

    var accessIndex = -1

    if (arrayIndex.isDefined)
      accessIndex = arrayIndex.get
    else if (dirAccess.isDefined)
      accessIndex = stencilField.findOffsetIndex(dirAccess.get.progress).get

    var numDims = Knowledge.dimensionality // TODO: resolve field info
    numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)

    if (accessIndex < 0)
      multiIndex(numDims - 1) = IR_IntegerConstant(0)
    else
      multiIndex(numDims - 1) = IR_IntegerConstant(accessIndex)

    val progOffset = if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      Some(progressedOffset)
    } else {
      None
    }

    if (accessIndex < 0)
      IR_StencilFieldAccess(IR_StencilFieldSelection(stencilField, stencilField.field.level, L4_FieldAccess.resolveSlot(stencilField.field, slot), None),
        multiIndex, progOffset)
    else
      IR_FieldAccess(IR_FieldSelection(stencilField.field, stencilField.field.level, L4_FieldAccess.resolveSlot(stencilField.field, slot), Some(accessIndex)),
        multiIndex, progOffset)
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
