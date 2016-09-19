package exastencils.field.l4

import exastencils.base.l4._
import exastencils.knowledge._
import exastencils.knowledge.l4.L4_HasIdentifierAndLevel
import exastencils.logger.Logger
import exastencils.prettyprinting._

case class L4_Field(
    var identifier : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var index : Int,
    var domain : String, // FIXME: l4.Domain
    var fieldLayout : L4_FieldLayout,
    var numSlots : Int,
    var boundary : Option[L4_Expression]) extends L4_HasIdentifierAndLevel {

  override def prettyprintDecl(out : PpStream) = {
    out << "Field " << identifier << "< "
    out << domain << ", "
    out << fieldLayout << ", "
    if (boundary.isDefined) out << boundary.get else out << "None"
    out << " >"
    if (numSlots > 1) out << "[" << numSlots << "]"
    out << "@" << level << "\n"
  }

  override def progress : Field = {
    progressed = Some(Field(
      identifier,
      index,
      DomainCollection.getDomainByIdentifier(domain).get,
      identifier.toLowerCase + "Data_" + level,
      fieldLayout.getProgressedObject,
      level,
      numSlots,
      L4_ProgressOption(boundary)(_.progress)))

    progressed.get
  }

  var progressed : Option[Field] = None
  override def getProgressedObject = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ identifier }")
    progressed.get
  }
}
