package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2._
import exastencils.datastructures._
import exastencils.domain.l2.L2_DomainCollection
import exastencils.logger._
import exastencils.prettyprinting._

/// L2_FieldDeclaration

trait L2_FieldDecl extends L2_Statement {
  def name : String
  override def progress = { Logger.error(s"Trying to progress l2 field declaration for field $name; this is not supported") }
}

/// L2_BaseFieldDeclaration

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

  override def prettyprint(out : PpStream) = {
    out << "--- FIXME ---"
  }
}

/// L2_BoundaryFieldDeclaration

case class L2_BoundaryFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var boundary : L2_BoundaryCondition) extends L2_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "--- FIXME ---"
  }
}

/// L2_ProcessFieldDeclarations

object L2_ProcessFieldDeclarations extends DefaultStrategy("Integrate Layer2 field declarations with knowledge") {
  this += Transformation("Process new fields", {
    case field : L2_BaseFieldDecl =>
      val levelList = L2_LevelSpecification.extractLevelListDefAll(field.levels)
      for (level <- levelList) {
        val newField = L2_Field(
          field.name,
          level,
          L2_DomainCollection.getByIdentifier(field.domain).get,
          field.datatype,
          field.localization,
          field.initial,
          L2_NoBC)

        L2_FieldCollection.add(newField)
      }

      None // consume declaration statement
  })

  this += Transformation("Adapt bc's of new fields", {
    case field : L2_BoundaryFieldDecl =>
      val levelList = L2_LevelSpecification.extractLevelListDefAll(field.levels)
      for (level <- levelList) {
        val fieldToAdapt = L2_FieldCollection.getByIdentifier(field.name, level).get
        fieldToAdapt.boundary = field.boundary
      }

      None // consume declaration statement
  })
}
