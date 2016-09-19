package exastencils.field.l4

import exastencils.base.l4._
import exastencils.knowledge._
import exastencils.knowledge.l4.L4_HasIdentifierAndLevel
import exastencils.prettyprinting._

case class L4_Field(
    var identifier : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var index : Int,
    var domain : String, // FIXME: l4.Domain
    var fieldLayout : String, // FIXME: l4.FieldLayout
    var numSlots : Int,
    var boundary : Option[L4_Expression]) extends L4_HasIdentifierAndLevel {

  def numDimsGrid = Knowledge.dimensionality // FIXME: number of dimensions should be variable

  def resolveLayout = L4_FieldLayoutCollection.getByIdentifier(fieldLayout, level).get

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
    Field(
      identifier,
      index,
      DomainCollection.getDomainByIdentifier(domain).get,
      identifier.toLowerCase + "Data_" + level,
      FieldLayoutCollection.getFieldLayoutByIdentifier(fieldLayout, level).get,
      level,
      numSlots,
      L4_ProgressOption(boundary)(_.progress))
  }
}
