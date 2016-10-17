package exastencils.interfacing.l4

import exastencils.field.l4._
import exastencils.interfacing.ir.IR_ExternalField
import exastencils.knowledge.l4.L4_KnowledgeObjectWithIdentAndLevel
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_ExternalField

object L4_ExternalField {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_ExternalField(
    var identifier : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var fieldLayout : L4_FieldLayout,
    var targetField : L4_Field // the (internal) field to be copied to/ from
) extends L4_KnowledgeObjectWithIdentAndLevel {

  override def prettyprintDecl(out : PpStream) = {
    out << "external Field " << identifier << " <" << fieldLayout << "> => " << targetField << "@" << level << '\n'
  }

  override def progress = {
    progressed = Some(IR_ExternalField(identifier, targetField.getProgressedObject, fieldLayout.getProgressedObject, level))
    progressed.get
  }

  var progressed : Option[IR_ExternalField] = None
  override def getProgressedObject = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ identifier }")
    progressed.get
  }
}
