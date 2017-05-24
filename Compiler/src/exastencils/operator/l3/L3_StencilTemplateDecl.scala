package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.boundary.l3.L3_NoBC
import exastencils.datastructures._
import exastencils.domain.l3._
import exastencils.field.l3._
import exastencils.knowledge.l3._
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_StencilTemplateDecl

object L3_StencilTemplateDecl {
  def apply(name : String, levels : Option[L3_LevelSpecification], localization : String, domainName : String, offsets : List[L3_Index]) =
    new L3_StencilTemplateDecl(name, levels, localization, domainName, offsets.to[ListBuffer])
}

case class L3_StencilTemplateDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var localization : String,
    var domainName : String,
    var offsets : ListBuffer[L3_Index]) extends L3_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name << " from StencilTemplate on " << localization << " of " << domainName << " {\n"
    for (offset <- offsets)
      out << offset << " =>\n"
    out << "}"
  }

  override def progress = Logger.error(s"Trying to progress L3 stencil template $name; this is not supported")
}

/// L3_PrepareStencilTemplateDeclaration

object L3_PrepareStencilTemplateDeclarations extends DefaultStrategy("Prepare knowledge for L3 stencil templates") {
  this += Transformation("Process new stencil templates", {
    case decl : L3_StencilTemplateDecl =>
      L3_FieldCollection.addDeclared(decl.name + "_Data", decl.levels)
      L3_StencilCollection.addDeclared(decl.name + "_Stencil", decl.levels)

      decl // preserve declaration statement
  })
}

/// L3_ProcessStencilTemplateDeclarations

object L3_ProcessStencilTemplateDeclarations extends DefaultStrategy("Integrate L3 stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case decl : L3_StencilTemplateDecl if !L3_FutureKnowledgeAccess.existsInStmt(decl) =>
      val level = L3_LevelSpecification.asSingleLevel(decl.levels)
      val domain = L3_DomainCollection.getByIdentifier(decl.domainName).get

      // TODO: warn for divergent lengths?
      val numDims = decl.offsets.map(_.length).max

      val field = L3_Field(decl.name + "_Data", level, domain,
        L3_VectorDatatype(L3_RealDatatype /*FIXME: decl.datatype*/ , decl.offsets.length),
        decl.localization, None, L3_NoBC)

      val stencil = L3_Stencil(decl.name + "_Stencil", level, numDims, Array.fill(numDims)(1.0),
        decl.offsets.zipWithIndex.map { case (offset, i) =>
          L3_StencilOffsetEntry(offset, L3_HigherDimSelection(L3_FieldAccess(field), L3_ConstIndex(i))).asStencilMappingEntry
        })

      L3_FieldCollection.add(field)
      L3_StencilCollection.add(stencil)
      //L3_StencilTemplateCollection.add(L3_StencilTemplate(decl.name, level, decl.localization, domain, decl.offsets)) // defer level determination

      None // consume declaration statement
  })
}
