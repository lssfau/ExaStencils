package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l3._
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_StencilTemplateDecl

object L3_StencilTemplateDecl {
  def apply(name : String, localization : String, domainName : String, offsets : List[L3_Index]) =
    new L3_StencilTemplateDecl(name, localization, domainName, offsets.to[ListBuffer])
}

case class L3_StencilTemplateDecl(
    var name : String,
    var localization : String,
    var domainName : String,
    var offsets : ListBuffer[L3_Index]) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name << " from StencilTemplate on " << localization << " of " << domainName << " {\n"
    for (offset <- offsets)
      out << offset << " =>\n"
    out << "}"
  }

  override def progress = Logger.error(s"Trying to progress l3 stencil template $name; this is not supported")
}

/// L3_ProcessStencilTemplateDeclarations

object L3_ProcessStencilTemplateDeclarations extends DefaultStrategy("Integrate Layer3 stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case stencil : L3_StencilTemplateDecl =>
      // TODO: how to choose levels?
      for (level <- Knowledge.levels) {
        val domain = L3_DomainCollection.getByIdentifier(stencil.domainName).get
        L3_StencilTemplateCollection.add(L3_StencilTemplate(stencil.name, level, stencil.localization, domain, stencil.offsets)) // defer level determination
      }
      None // consume declaration statement
  })
}
