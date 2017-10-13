package exastencils.field.l3

import exastencils.base.l3._
import exastencils.field.l4._
import exastencils.prettyprinting._

/// L3_SlotSpecification

trait L3_SlotSpecification extends L3_Node with L3_Progressable with PrettyPrintable {
  override def progress : L4_SlotSpecification
}

/// L3_ActiveSlot

case object L3_ActiveSlot extends L3_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "active"
  override def progress = L4_ActiveSlot
}

/// L3_NextSlot

case object L3_NextSlot extends L3_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "next"
  override def progress = L4_NextSlot
}

/// L3_PreviousSlot

case object L3_PreviousSlot extends L3_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "previous"
  override def progress = L4_PreviousSlot
}

/// L3_ConstantSlot

case class L3_ConstantSlot(slot : Long) extends L3_SlotSpecification {
  override def prettyprint(out : PpStream) = out << slot
  override def progress = L4_ConstantSlot(slot)
}

/// L3_AdvanceSlot

case class L3_AdvanceSlot(var access : L3_Access) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << access
  override def progress = L4_AdvanceSlot(access.progress)
}
