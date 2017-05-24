package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilTemplateDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilTemplateDecl.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import scala.collection.mutable._

import exastencils.base.|LAYER_LC|._
import exastencils.baseExt.|LAYER_LC|._
import exastencils.boundary.|LAYER_LC|.|LAYER_UC|_NoBC
import exastencils.datastructures._
import exastencils.domain.|LAYER_LC|._
import exastencils.field.|LAYER_LC|._
import exastencils.knowledge.|LAYER_LC|._
import exastencils.logger._
import exastencils.prettyprinting._

/// |LAYER_UC|_StencilTemplateDecl

object |LAYER_UC|_StencilTemplateDecl {
  def apply(name : String, levels : Option[|LAYER_UC|_LevelSpecification], localization : String, domainName : String, offsets : List[|LAYER_UC|_Index]) =
    new |LAYER_UC|_StencilTemplateDecl(name, levels, localization, domainName, offsets.to[ListBuffer])
}

case class |LAYER_UC|_StencilTemplateDecl(
    var name : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var localization : String,
    var domainName : String,
    var offsets : ListBuffer[|LAYER_UC|_Index]) extends |LAYER_UC|_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name << " from StencilTemplate on " << localization << " of " << domainName << " {\n"
    for (offset <- offsets)
      out << offset << " =>\n"
    out << "}"
  }

  override def progress = Logger.error(s"Trying to progress |LAYER_UC| stencil template $name; this is not supported")
}

/// |LAYER_UC|_PrepareStencilTemplateDeclaration

object |LAYER_UC|_PrepareStencilTemplateDeclarations extends DefaultStrategy("Prepare knowledge for |LAYER_UC| stencil templates") {
  this += Transformation("Process new stencil templates", {
    case decl : |LAYER_UC|_StencilTemplateDecl =>
      |LAYER_UC|_FieldCollection.addDeclared(decl.name + "_Data", decl.levels)
      |LAYER_UC|_StencilCollection.addDeclared(decl.name + "_Stencil", decl.levels)

      decl // preserve declaration statement
  })
}

/// |LAYER_UC|_ProcessStencilTemplateDeclarations

object |LAYER_UC|_ProcessStencilTemplateDeclarations extends DefaultStrategy("Integrate |LAYER_UC| stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case decl : |LAYER_UC|_StencilTemplateDecl if !|LAYER_UC|_FutureKnowledgeAccess.existsInStmt(decl) =>
      val level = |LAYER_UC|_LevelSpecification.asSingleLevel(decl.levels)
      val domain = |LAYER_UC|_DomainCollection.getByIdentifier(decl.domainName).get

      // TODO: warn for divergent lengths?
      val numDims = decl.offsets.map(_.length).max

      val field = |LAYER_UC|_Field(decl.name + "_Data", level, domain,
        |LAYER_UC|_VectorDatatype(|LAYER_UC|_RealDatatype /*FIXME: decl.datatype*/ , decl.offsets.length),
        decl.localization, None, |LAYER_UC|_NoBC)

      val stencil = |LAYER_UC|_Stencil(decl.name + "_Stencil", level, numDims, Array.fill(numDims)(1.0),
        decl.offsets.zipWithIndex.map { case (offset, i) =>
          |LAYER_UC|_StencilOffsetEntry(offset, |LAYER_UC|_HigherDimSelection(|LAYER_UC|_FieldAccess(field), |LAYER_UC|_ConstIndex(i))).asStencilMappingEntry
        })

      |LAYER_UC|_FieldCollection.add(field)
      |LAYER_UC|_StencilCollection.add(stencil)
      //|LAYER_UC|_StencilTemplateCollection.add(|LAYER_UC|_StencilTemplate(decl.name, level, decl.localization, domain, decl.offsets)) // defer level determination

      None // consume declaration statement
  })
}
"""
  }
}
