package exastencils.interfacing.l4

import exastencils.field.l4._
import exastencils.interfacing.ir.IR_ExternalField
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.prettyprinting._

/// L4_ExternalField

object L4_ExternalField {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_ExternalField(
    var name : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var fieldLayout : L4_FieldLayout,
    var targetField : L4_Field // the (internal) field to be copied to/ from
) extends L4_KnowledgeObjectWithLevel[IR_ExternalField] {

  override def prettyprintDecl(out : PpStream) = {
    out << "external Field " << name << " <" << fieldLayout << "> => " << targetField << "@" << level
  }

  override def progressImpl() = IR_ExternalField(name, targetField.getProgressedObject(), fieldLayout.getProgressedObject(), level)
}
