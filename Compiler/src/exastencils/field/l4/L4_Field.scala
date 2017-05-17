package exastencils.field.l4

import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir.IR_Field
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.prettyprinting._

/// L4_Field

case class L4_Field(
    var name : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var index : Int,
    var domainName : String, // FIXME: var domain : L4_Domain
    var fieldLayout : L4_FieldLayout,
    var numSlots : Int,
    var boundary : L4_BoundaryCondition) extends L4_LeveledKnowledgeObject[IR_Field] {

  def datatype = fieldLayout.datatype

  // FIXME: handle numDimsGrid
  def numDimsGrid = Knowledge.dimensionality

  override def prettyprintDecl(out : PpStream) = {
    out << "Field " << name
    out << "< " << domainName << ", " << fieldLayout.name << ", " << boundary << " >"
    if (numSlots > 1) out << "[" << numSlots << "]"
    out << "@" << level
  }

  override def progressImpl() = {
    IR_Field(
      name,
      level,
      index,
      IR_DomainCollection.getByIdentifier(domainName).get,
      name.toLowerCase + "Data_" + level,
      fieldLayout.getProgressedObject(),
      numSlots,
      boundary.progress)
  }
}
