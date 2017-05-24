package exastencils.field.l4

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.field.ir._
import exastencils.logger.Logger
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
  override def prettyprint(out : PpStream) = {
    // don't use field's prettyprinter -> slot modifiers are not required in advance statements
    field match {
      case access : L4_FieldAccess =>
        out << "advance " << access.target.name << "@" << access.target.level

      case access : L4_UnresolvedAccess =>
        out << "advance " << field.name
        if (access.level.isDefined) out << "@" << access.level.get

      case access =>
        Logger.warn("Trying to advance slot of something that is not a field: " + access)
        out << "advance " << access
    }
  }

  override def progress = {
    IR_AdvanceSlot(IR_IV_ActiveSlot(field.asInstanceOf[L4_FieldAccess].target.getProgressedObj(),
      IR_LoopOverFragments.defIt))
  }
}
