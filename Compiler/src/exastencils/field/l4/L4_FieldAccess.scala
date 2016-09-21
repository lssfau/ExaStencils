package exastencils.field.l4

import exastencils._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.datastructures.ir.iv
import exastencils.datastructures.l4._
import exastencils.datastructures.{ ir => _, _ }
import exastencils.field.ir.IR_FieldAccess
import exastencils.knowledge._
import exastencils.knowledge.l4.L4_KnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FieldAccess

object L4_FieldAccess {
  def apply(fieldName : String, level : Int, slot : L4_SlotSpecification, arrayIndex : Option[Int], offset : Option[L4_ExpressionIndex]) =
    new L4_FieldAccess(L4_FieldCollection.getByIdentifier(fieldName, level).get, slot, arrayIndex, offset)

  def resolveSlot(field : Field, slot : L4_SlotSpecification) = {
    if (1 == field.numSlots) IR_IntegerConstant(0)
    else slot match {
      case L4_ActiveSlot       => data.SlotAccess(iv.CurrentSlot(field), 0)
      case L4_NextSlot         => data.SlotAccess(iv.CurrentSlot(field), 1)
      case L4_PreviousSlot     => data.SlotAccess(iv.CurrentSlot(field), -1)
      case x : L4_ConstantSlot => IR_IntegerConstant(x.number)
      case _                   => Logger.error("Unknown slot modifier " + slot)
    }
  }
}

case class L4_FieldAccess(
    var target : L4_Field,
    var slot : L4_SlotSpecification,
    var arrayIndex : Option[Int] = None,
    var offset : Option[L4_ExpressionIndex] = None) extends L4_KnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << target.identifier
    if (target.numSlots > 1) out << '[' << slot << ']'
    out << '@' << target.level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset.get
  }

  def progress : IR_FieldAccess = {
    // TODO: extract common index stuff from here and VirtualFieldAccess, StencilFieldAccess, etc
    var numDims = Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    val field = target.getProgressedObject
    IR_FieldAccess(FieldSelection(field, IR_IntegerConstant(field.level), L4_FieldAccess.resolveSlot(field, slot), arrayIndex), multiIndex)
  }
}

/// L4_ResolveFieldAccesses

object L4_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : UnresolvedAccess if L4_FieldCollection.exists(access.name) =>
      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field - was an offset access (@) intended?")
      L4_FieldAccess(access.name, access.level.get.resolveLevel, access.slot.getOrElse(L4_ActiveSlot), access.arrayIndex, access.offset)
  })
}