package exastencils.stencil.l4

import exastencils.base.ir._
import exastencils.base.l4.L4_ExpressionIndex
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.l4._
import exastencils.knowledge._
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
    out << target.identifier
    if (target.field.numSlots > 1) out << '[' << slot << ']'
    out << '@' << target.level
    if (offset.isDefined) out << "@" << offset
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ":" << dirAccess
  }

  @deprecated("to be deleted", "20.09.2016")
  def resolveField = target.getProgressedObject.field

  @deprecated("to be deleted", "20.09.2016")
  def getBasicStencilFieldAccess : IR_StencilFieldAccess = {
    if (arrayIndex.isDefined || dirAccess.isDefined)
      Logger.warn(s"Discarding modifiers of access to stencilfield ${ target.identifier } on level ${ target.level }")

    val stencilField = target.getProgressedObject

    var numDims = stencilField.field.fieldLayout.numDimsGrid
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    IR_StencilFieldAccess(StencilFieldSelection(stencilField, IR_IntegerConstant(stencilField.field.level), L4_FieldAccess.resolveSlot(stencilField.field, slot), None), multiIndex)
  }

  def progress : IR_Expression = {
    // TODO: extract mapping to FieldAccess for cases where single entries are targeted into a separate strategy

    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencil field ${ target.identifier } on level ${ target.level } has direction access and array subscript modifiers; array index will be given precedence, offset will be ignored")

    val stencilField = target.getProgressedObject

    var accessIndex = -1

    if (arrayIndex.isDefined)
      accessIndex = arrayIndex.get
    else if (dirAccess.isDefined)
      accessIndex = stencilField.stencil.findStencilEntryIndex(dirAccess.get.progress).get

    var numDims = Knowledge.dimensionality // TODO: resolve field info
    numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)

    if (accessIndex < 0)
      multiIndex(numDims - 1) = IR_IntegerConstant(0)
    else
      multiIndex(numDims - 1) = IR_IntegerConstant(accessIndex)

    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    if (accessIndex < 0)
      IR_StencilFieldAccess(StencilFieldSelection(stencilField, IR_IntegerConstant(stencilField.field.level), L4_FieldAccess.resolveSlot(stencilField.field, slot), None),
        multiIndex)
    else
      IR_FieldAccess(FieldSelection(stencilField.field, IR_IntegerConstant(stencilField.field.level), L4_FieldAccess.resolveSlot(stencilField.field, slot), Some(accessIndex)),
        multiIndex)
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
