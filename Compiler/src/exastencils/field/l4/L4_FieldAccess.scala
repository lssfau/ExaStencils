package exastencils.field.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4._
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FieldAccess

object L4_FieldAccess {
  def apply(access : L4_FutureFieldAccess) =
    new L4_FieldAccess(L4_FieldCollection.getByIdentifier(access.name, access.level).get, access.slot, access.offset, access.arrayIndex, access.frozen)

  def resolveSlot(field : IR_Field, slot : L4_SlotSpecification) = {
    if (1 == field.numSlots) IR_IntegerConstant(0)
    else slot match {
      case L4_ActiveSlot       => IR_SlotAccess(IR_IV_ActiveSlot(field), 0)
      case L4_NextSlot         => IR_SlotAccess(IR_IV_ActiveSlot(field), 1)
      case L4_PreviousSlot     => IR_SlotAccess(IR_IV_ActiveSlot(field), -1)
      case x : L4_ConstantSlot => IR_IntegerConstant(x.number)
      case _                   => Logger.error("Unknown slot modifier " + slot)
    }
  }
}

case class L4_FieldAccess(
    var target : L4_Field,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var arrayIndex : Option[Int] = None,
    var frozen : Boolean = false) extends L4_LeveledKnowledgeAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << target.name
    if (target.numSlots > 1) out << '[' << slot << ']'
    out << '@' << target.level
    if (offset.isDefined) out << "@" << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (frozen) out << " )"
  }

  def getOffset = offset.getOrElse(L4_ConstIndex(Array.fill(target.numDimsGrid)(0)))

  override def progress : IR_FieldAccess = ProgressLocation {
    val field = target.getProgressedObj()

    val numDims = field.fieldLayout.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)

    if (arrayIndex.isDefined)
      target.fieldLayout.datatype match {
        case v : L4_VectorDatatype if v.isRow  =>
          index.indices :+= IR_IntegerConstant(0)
          index.indices :+= IR_IntegerConstant(arrayIndex.get)
        case v : L4_VectorDatatype if !v.isRow =>
          index.indices :+= IR_IntegerConstant(arrayIndex.get)
          index.indices :+= IR_IntegerConstant(0)
        case other                             => Logger.warn(s"Ignoring component access for unsupported datatype $other")
      }

    val progOffset = if (offset.isDefined) {
      val progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < index.length) progressedOffset.indices :+= 0
      Some(progressedOffset)
    } else {
      None
    }

    IR_FieldAccess(IR_FieldSelection(field, field.level, L4_FieldAccess.resolveSlot(field, slot)), index, progOffset, frozen)
  }
}

/// L4_ResolveFieldAccesses

object L4_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureFieldAccess if L4_FieldCollection.exists(access.name, access.level) =>
      access.toFieldAccess
  })
}

/// L4_UnresolveFieldAccesses

object L4_UnresolveFieldAccesses extends DefaultStrategy("Revert field accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_FieldAccess(target, slot, offset, arrayIndex, frozen) =>
      val newSlot = if (L4_ActiveSlot == slot) None else Some(slot)
      def ret = L4_UnresolvedAccess(target.name, Some(L4_SingleLevel(target.level)), newSlot, offset, None, arrayIndex)
      if (frozen)
        L4_FunctionCall(L4_UnresolvedFunctionReference("frozen", None, None), ret)
      else
        ret
  })
}
