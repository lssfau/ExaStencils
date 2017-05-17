package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2._
import exastencils.datastructures._
import exastencils.domain.l2.L2_DomainCollection
import exastencils.knowledge.l2._
import exastencils.logger._
import exastencils.prettyprinting._

/// L2_FieldDeclaration

abstract class L2_FieldDecl extends L2_LeveledKnowledgeDecl {
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

/// L2_UnfoldStencilDeclarations

object L2_UnfoldFieldDeclarations extends DefaultStrategy("Unfold L2 field declarations") {
  this += Transformation("Process new stencils", {
    case decl : L2_BaseFieldDecl     => L2_LeveledKnowledgeDecl.unfoldDecl(decl)
    case decl : L2_BoundaryFieldDecl => L2_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}

/// L2_PrepareFieldDeclaration

object L2_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L2 fields") {
  this += Transformation("Process new fields", {
    case decl : L2_BaseFieldDecl =>
      L2_FieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L2_ProcessFieldDeclarations

object L2_ProcessFieldDeclarations extends DefaultStrategy("Integrate L2 field declarations with knowledge") {
  this += Transformation("Process new fields", {
    case decl : L2_BaseFieldDecl if !L2_FutureKnowledgeAccess.existsInStmt(decl) =>
      val field = L2_Field(
        decl.name,
        L2_LevelSpecification.asSingleLevel(decl.levels),
        L2_DomainCollection.getByIdentifier(decl.domain).get,
        decl.datatype,
        decl.localization,
        decl.initial,
        L2_NoBC)

      L2_FieldCollection.add(field)

      None // consume declaration statement
  })

  this += Transformation("Adapt bc's of new fields", {
    case decl : L2_BoundaryFieldDecl if !L2_FutureKnowledgeAccess.existsInStmt(decl) =>
      val fieldToAdapt = L2_FieldCollection.getByIdentifier(decl.name, L2_LevelSpecification.asSingleLevel(decl.levels)).get
      fieldToAdapt.boundary = decl.boundary

      None // consume declaration statement
  })
}
