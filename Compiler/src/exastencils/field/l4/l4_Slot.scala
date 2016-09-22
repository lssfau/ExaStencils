package exastencils.field.l4

import exastencils.base.ir.IR_StringLiteral
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.ir.iv
import exastencils.field.ir.IR_AdvanceSlot
import exastencils.prettyprinting._

/// L4_SlotSpecification

trait L4_SlotSpecification extends L4_Node with PrettyPrintable {}

/// L4_ActiveSlot
case object L4_ActiveSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "activeSlot"
}

/// L4_NextSlot

case object L4_NextSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "nextSlot"
}

/// L4_PreviousSlot

case object L4_PreviousSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "previousSlot"
}

/// L4_ConstantSlot

case class L4_ConstantSlot(number : Long) extends L4_SlotSpecification {
  override def prettyprint(out : PpStream) = out << number
}

/// L4_AdvanceSlot

case class L4_AdvanceSlot(var field : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "advance " << field << '\n'

  override def progress = {
    IR_AdvanceSlot(iv.CurrentSlot(field.asInstanceOf[L4_FieldAccess].target.getProgressedObject,
      IR_StringLiteral(IR_LoopOverFragments.defIt)))
  }
}
