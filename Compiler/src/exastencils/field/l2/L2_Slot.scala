package exastencils.field.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.field.l3._
import exastencils.prettyprinting._

/// L2_SlotSpecification

trait L2_SlotSpecification extends L2_Node with L2_Progressable with PrettyPrintable {
  override def progress : L3_SlotSpecification
}

/// L2_ActiveSlot

case object L2_ActiveSlot extends L2_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "active"
  override def progress = ProgressLocation(L3_ActiveSlot)
}

/// L2_NextSlot

case object L2_NextSlot extends L2_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "next"
  override def progress = ProgressLocation(L3_NextSlot)
}

/// L2_PreviousSlot

case object L2_PreviousSlot extends L2_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "previous"
  override def progress = ProgressLocation(L3_PreviousSlot)
}

/// L2_ConstantSlot

case class L2_ConstantSlot(slot : Long) extends L2_SlotSpecification {
  override def prettyprint(out : PpStream) = out << slot
  override def progress = ProgressLocation(L3_ConstantSlot(slot))
}

/// L2_AdvanceSlot

case class L2_AdvanceSlot(var access : L2_Access) extends L2_Statement {
  override def prettyprint(out : PpStream) = out << access
  override def progress = ProgressLocation(L3_AdvanceSlot(access.progress))
}
