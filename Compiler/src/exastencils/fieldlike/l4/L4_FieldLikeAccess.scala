package exastencils.fieldlike.l4

import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.field.ir.IR_SlotAccess
import exastencils.field.l4.L4_ActiveSlot
import exastencils.field.l4.L4_ConstantSlot
import exastencils.field.l4.L4_NextSlot
import exastencils.field.l4.L4_PreviousSlot
import exastencils.field.l4.L4_SlotSpecification
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

object L4_FieldLikeAccess {
  def apply(access : L4_FutureFieldLikeAccess) = {
    val target = L4_FieldLikeCollections.getByIdentifier(access.name, access.level).get
    target.getFieldAccess(access.slot, access.offset, access.frozen, access.matIndex)
  }

  def apply(target : L4_FieldLike[_, _], slot : L4_SlotSpecification, offset : Option[L4_ConstIndex] = None, frozen : Boolean = false, matIndex : Option[L4_MatIndex] = None) =
    target.getFieldAccess(slot, offset, frozen, matIndex)

  def resolveSlot(field : IR_FieldLike, slot : L4_SlotSpecification) = {
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

trait L4_FieldLikeAccess extends L4_LeveledKnowledgeAccess with L4_CanBeOffset {
  def target : L4_FieldLike[_, _]
  def slot : L4_SlotSpecification
  def offset : Option[L4_ConstIndex]
  def frozen : Boolean
  def matIndex : Option[L4_MatIndex]

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << target.name
    if (target.numSlots > 1) out << '<' << slot << '>'
    out << '@' << target.level
    if (offset.isDefined) out << "@" << offset.get
    if(matIndex.isDefined) {
      out << matIndex.get
    }
    if (frozen) out << " )"

  }

  def getOffset = offset.getOrElse(L4_ConstIndex(Array.fill(target.numDimsGrid)(0)))

  def progress : IR_FieldLikeAccess
}

/// L4_UnresolveFieldAccesses

object L4_UnresolveFieldLikeAccesses extends DefaultStrategy("Revert field accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case fAcc : L4_FieldLikeAccess =>
      val newSlot = if (L4_ActiveSlot == fAcc.slot) None else Some(fAcc.slot)
      def ret = L4_UnresolvedAccess(fAcc.target.name, Some(L4_SingleLevel(fAcc.target.level)), newSlot, fAcc.offset, None, fAcc.matIndex)
      if (fAcc.frozen)
        L4_FunctionCall(L4_UnresolvedFunctionReference("frozen", None, None), ret)
      else
        ret
  })
}
