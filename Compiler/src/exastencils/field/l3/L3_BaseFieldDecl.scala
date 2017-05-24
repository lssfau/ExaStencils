package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_NoBC
import exastencils.domain.l3.L3_DomainCollection
import exastencils.prettyprinting._

/// L3_BaseFieldDecl

object L3_BaseFieldDecl {
  def apply(identifier : String, levels : Option[L3_LevelSpecification], datatype : Option[L3_Datatype], localization : String, domain : String, initial : Option[L3_Expression]) : L3_BaseFieldDecl =
    L3_BaseFieldDecl(identifier, levels, datatype.getOrElse(L3_RealDatatype), localization, domain, initial)
}

case class L3_BaseFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var datatype : L3_Datatype,
    var localization : String,
    var domain : String,
    var initial : Option[L3_Expression]) extends L3_FieldDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    L3_FieldCollection.add(
      L3_Field(
        name,
        L3_LevelSpecification.asSingleLevel(levels),
        L3_DomainCollection.getByIdentifier(domain).get,
        datatype,
        localization,
        initial,
        L3_NoBC))
  }
}
