package exastencils.field.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.boundary.l4._
import exastencils.domain.l4._
import exastencils.prettyprinting._

/// L4_BaseFieldDecl

object L4_BaseFieldDecl {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], domainName : String, fieldLayoutName : String, boundary : L4_BoundaryCondition, numSlots : Integer) =
    new L4_BaseFieldDecl(name, levels, L4_UnresolvedAccess(domainName), L4_UnresolvedAccess(fieldLayoutName), boundary, numSlots)
}

case class L4_BaseFieldDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var domain : L4_Access,
    var fieldLayout : L4_Access,
    var boundary : L4_BoundaryCondition,
    var numSlots : Integer) extends L4_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name << "< " << domain.name << ", " << fieldLayout.name << ", " << boundary << " >"
    if (numSlots > 1) out << '[' << numSlots << ']'
    out << '@' << levels.get
  }

  override def addToKnowledge() : Unit = {
    val index = L4_FieldDecl.runningIndex
    L4_FieldDecl.runningIndex += 1

    L4_FieldCollection.add(
      L4_Field(name, levels.get.resolveLevel,
        index,
        domain.asInstanceOf[L4_DomainAccess].target,
        fieldLayout.asInstanceOf[L4_FieldLayoutAccess].target,
        numSlots,
        boundary))
  }
}
