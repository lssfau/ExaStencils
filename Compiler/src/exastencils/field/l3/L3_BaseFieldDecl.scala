package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_NoBC
import exastencils.domain.l3._
import exastencils.grid.l3.L3_Localization
import exastencils.prettyprinting._

/// L3_BaseFieldDecl

object L3_BaseFieldDecl {
  def apply(identifier : String, levels : Option[L3_LevelSpecification], datatype : Option[L3_Datatype], localization : String, domain : String, initial : Option[L3_Expression]) : L3_BaseFieldDecl =
    L3_BaseFieldDecl(identifier, levels, datatype.getOrElse(L3_RealDatatype), L3_Localization.resolve(localization), L3_FutureDomainAccess(domain), initial)
}

case class L3_BaseFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var datatype : L3_Datatype,
    var localization : L3_Localization,
    var domain : L3_Access,
    var initial : Option[L3_Expression]) extends L3_FieldDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    L3_FieldCollection.add(
      L3_Field(
        name,
        L3_LevelSpecification.asSingleLevel(levels),
        domain.asInstanceOf[L3_DomainAccess].target,
        datatype,
        localization,
        initial,
        L3_NoBC))
  }
}
