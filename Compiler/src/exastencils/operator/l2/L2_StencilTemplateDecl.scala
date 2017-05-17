package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.domain.l2._
import exastencils.knowledge.l2._
import exastencils.logger._
import exastencils.prettyprinting._

/// L2_StencilTemplateDecl

object L2_StencilTemplateDecl {
  def apply(name : String, levels : Option[L2_LevelSpecification], localization : String, domainName : String, offsets : List[L2_Index]) =
    new L2_StencilTemplateDecl(name, levels, localization, domainName, offsets.to[ListBuffer])
}

case class L2_StencilTemplateDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var localization : String,
    var domainName : String,
    var offsets : ListBuffer[L2_Index]) extends L2_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name << " from StencilTemplate on " << localization << " of " << domainName << " {\n"
    for (offset <- offsets)
      out << offset << " =>\n"
    out << "}"
  }

  override def progress = Logger.error(s"Trying to progress l2 stencil template $name; this is not supported")
}

/// L2_UnfoldStencilTemplateDeclarations

object L2_UnfoldStencilTemplateDeclarations extends DefaultStrategy("Unfold L2 field declarations") {
  this += Transformation("Process new stencil templates", {
    case decl : L2_StencilTemplateDecl => L2_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}

/// L2_ProcessStencilTemplateDeclarations

object L2_ProcessStencilTemplateDeclarations extends DefaultStrategy("Integrate L2 stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case decl : L2_StencilTemplateDecl if !L2_FutureKnowledgeAccess.existsInStmt(decl) =>
      val level = L2_LevelSpecification.asSingleLevel(decl.levels)
      val domain = L2_DomainCollection.getByIdentifier(decl.domainName).get
      L2_StencilTemplateCollection.add(L2_StencilTemplate(decl.name, level, decl.localization, domain, decl.offsets)) // defer level determination

      None // consume declaration statement
  })
}
