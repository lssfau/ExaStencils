package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2.L2_NoBC
import exastencils.domain.l2.L2_DomainCollection
import exastencils.prettyprinting._

/// L2_BaseFieldDecl

object L2_BaseFieldDecl {
  def apply(identifier : String, levels : Option[L2_LevelSpecification], datatype : Option[L2_Datatype], localization : String, domain : String, initial : Option[L2_Expression]) : L2_BaseFieldDecl =
    L2_BaseFieldDecl(identifier, levels, datatype.getOrElse(L2_RealDatatype), localization, domain, initial)
}

case class L2_BaseFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var datatype : L2_Datatype,
    var localization : String,
    var domain : String,
    var initial : Option[L2_Expression]) extends L2_FieldDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    L2_FieldCollection.add(
      L2_Field(
        name,
        L2_LevelSpecification.asSingleLevel(levels),
        L2_DomainCollection.getByIdentifier(domain).get,
        datatype,
        localization,
        initial,
        L2_NoBC))
  }
}
