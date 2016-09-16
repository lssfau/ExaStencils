package exastencils.field.l4

import exastencils.base.l4.L4_Node
import exastencils.prettyprinting._

trait L4_SlotSpecification extends L4_Node with PrettyPrintable {}

case object L4_ActiveSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "activeSlot"
}

case object L4_NextSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "nextSlot"
}

case object L4_PreviousSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "previousSlot"
}

case class L4_ConstantSlot(number : Long) extends L4_SlotSpecification {
  override def prettyprint(out : PpStream) = out << number
}
