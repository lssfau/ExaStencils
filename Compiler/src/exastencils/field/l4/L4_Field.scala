package exastencils.field.l4

import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.domain.l4.L4_Domain
import exastencils.field.ir.IR_Field
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.prettyprinting._

/// L4_Field

case class L4_Field(
    var name : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var index : Int,
    var domain : L4_Domain,
    var fieldLayout : L4_FieldLayout,
    var numSlots : Int,
    var boundary : L4_BoundaryCondition) extends L4_LeveledKnowledgeObject[IR_Field] {

  def datatype = fieldLayout.datatype

  override def prettyprintDecl(out : PpStream) = {
    out << "Field " << name
    out << "< " << domain.name << ", " << fieldLayout.name << ", " << boundary << " >"
    if (numSlots > 1) out << "[" << numSlots << "]"
    out << "@" << level
  }

  def codeName = name + "_" + level
  def numDimsGrid = domain.numDims

  override def progressImpl() = {
    IR_Field(
      name,
      level,
      index,
      domain.getProgressedObj(),
      name.toLowerCase + "Data_" + level,
      fieldLayout.getProgressedObj(),
      numSlots,
      boundary.progress)
  }
}
