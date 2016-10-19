package exastencils.field.l3

import exastencils.base.l3._
import exastencils.field.l4._
import exastencils.prettyprinting.PpStream

/// L3_FieldAccess

case class L3_FieldAccess(
    var field : L3_Field,
    var level : L3_AccessLevelSpecification) extends L3_Access {

  override def name = field.name
  override def prettyprint(out : PpStream) = out << name << '@' << level
  override def progress = L4_FieldAccess(field.getProgressedObject(), L4_ActiveSlot)
}
