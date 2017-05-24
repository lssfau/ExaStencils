package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.boundary.l2.L2_NoBC
import exastencils.datastructures._
import exastencils.domain.l2._
import exastencils.field.l2._
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

  override def progress = Logger.error(s"Trying to progress L2 stencil template $name; this is not supported")
}

/// L2_PrepareStencilTemplateDeclaration

object L2_PrepareStencilTemplateDeclarations extends DefaultStrategy("Prepare knowledge for L2 stencil templates") {
  this += Transformation("Process new stencil templates", {
    case decl : L2_StencilTemplateDecl =>
      L2_FieldCollection.addDeclared(decl.name + "_Data", decl.levels)
      L2_StencilCollection.addDeclared(decl.name + "_Stencil", decl.levels)

      decl // preserve declaration statement
  })
}

/// L2_ProcessStencilTemplateDeclarations

object L2_ProcessStencilTemplateDeclarations extends DefaultStrategy("Integrate L2 stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case decl : L2_StencilTemplateDecl if !L2_FutureKnowledgeAccess.existsInStmt(decl) =>
      val level = L2_LevelSpecification.asSingleLevel(decl.levels)
      val domain = L2_DomainCollection.getByIdentifier(decl.domainName).get

      // TODO: warn for divergent lengths?
      val numDims = decl.offsets.map(_.length).max

      val field = L2_Field(decl.name + "_Data", level, domain,
        L2_VectorDatatype(L2_RealDatatype /*FIXME: decl.datatype*/ , decl.offsets.length),
        decl.localization, None, L2_NoBC)

      val stencil = L2_Stencil(decl.name + "_Stencil", level, numDims, Array.fill(numDims)(1.0),
        decl.offsets.zipWithIndex.map { case (offset, i) =>
          L2_StencilOffsetEntry(offset, L2_HigherDimSelection(L2_FieldAccess(field), L2_ConstIndex(i))).asStencilMappingEntry
        })

      L2_FieldCollection.add(field)
      L2_StencilCollection.add(stencil)
      //L2_StencilTemplateCollection.add(L2_StencilTemplate(decl.name, level, decl.localization, domain, decl.offsets)) // defer level determination

      None // consume declaration statement
  })
}
