package exastencils.field.l4

import exastencils.base.l4._
import exastencils.boundary.l4._
import exastencils.prettyprinting._

/// L4_BaseFieldDecl

case class L4_BaseFieldDecl(
    var name : String,
    var levels : Option[L4_LevelSpecification],
    var domainName : String,
    var fieldLayoutName : String,
    var boundary : L4_BoundaryCondition,
    var numSlots : Integer,
    var index : Int = 0) extends L4_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name << "< " << domainName << ", " << fieldLayoutName << ", " << boundary << " >"
    if (numSlots > 1) out << '[' << numSlots << ']'
    out << '@' << levels
  }

  override def addToKnowledge() : Unit = {
    val level = levels.get.asInstanceOf[L4_SingleLevel].level
    def index = L4_FieldDecl.runningIndex
    L4_FieldDecl.runningIndex += 1

    val resolvedFieldLayout = L4_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get

    L4_FieldCollection.add(L4_Field(name, level, index, domainName, resolvedFieldLayout, numSlots, boundary))
  }
}
