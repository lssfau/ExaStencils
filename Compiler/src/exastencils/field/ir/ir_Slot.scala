package exastencils.field.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.ir.iv
import exastencils.prettyprinting.PpStream

case class IR_AdvanceSlot(var slot : iv.CurrentSlot) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  // slot never contains negative values (currently)
  def expandSpecial = IR_Assignment(slot, (slot + 1) Mod slot.field.numSlots)
}
