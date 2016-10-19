package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.field.l4._
import exastencils.knowledge.l3.L3_KnowledgeObjectWithLevel
import exastencils.prettyprinting.PpStream

/// L3_Field

object L3_Field {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L3_Field(
    var name : String,
    var level : Int,
    var domainName : String, // TODO: var domain : L3_Domain
    var datatype : L3_Datatype,
    var localization : String,
    var initial : Option[L3_Expression],
    var boundary : L3_BoundaryCondition) extends L3_KnowledgeObjectWithLevel[L4_Field] {

  def fieldLayoutName = s"defLayout$localization"
  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = {
    L4_Field(
      name,
      level,
      -1, // index is to be set later
      domainName,
      L4_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get, // l3 field layout is not available -> grab l4 layout directly
      1, // one slot by default - may be increased later
      boundary.progress)
  }
}
