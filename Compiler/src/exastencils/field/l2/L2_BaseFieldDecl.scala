package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2.L2_NoBC
import exastencils.domain.l2._
import exastencils.grid.l2.L2_Localization
import exastencils.prettyprinting._

/// L2_BaseFieldDecl

object L2_BaseFieldDecl {
  def apply(identifier : String, levels : Option[L2_LevelSpecification], datatype : Option[L2_Datatype], localization : String, domain : String, initial : Option[L2_Expression]) : L2_BaseFieldDecl =
    L2_BaseFieldDecl(identifier, levels, datatype.getOrElse(L2_RealDatatype), L2_Localization.resolve(localization), L2_FutureDomainAccess(domain), initial)
}

case class L2_BaseFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var datatype : L2_Datatype,
    var localization : L2_Localization,
    var domain : L2_Access,
    var initial : Option[L2_Expression]) extends L2_FieldDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    L2_FieldCollection.add(
      L2_Field(
        name,
        L2_LevelSpecification.asSingleLevel(levels),
        domain.asInstanceOf[L2_DomainAccess].target,
        datatype,
        localization,
        initial,
        L2_NoBC))
  }
}
