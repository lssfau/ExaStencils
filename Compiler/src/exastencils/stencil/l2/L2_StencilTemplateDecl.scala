package exastencils.stencil.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l2._
import exastencils.logger._
import exastencils.prettyprinting._

///// L2_StencilTemplateDecl
//
//case class L2_StencilTemplateDecl(
//    var name : String,
//    var localization : String,
//    var domainName : String,
//    var offsets : ListBuffer[L2_Index]) extends L2_Statement {
//
//  override def prettyprint(out : PpStream) = {
//    out << "StencilTemplate " << name << " {\n"
//    for (offset <- offsets)
//      out << offset << " =>\n"
//    out << "}\n"
//  }
//
//  override def progress = Logger.error(s"Trying to progress l2 stencil template $name; this is not supported")
//}
//
///// L2_ProcessStencilTemplateDeclaration
//
//object L2_ProcessStencilTemplateDeclaration extends DefaultStrategy("Integrate Layer2 stencil template declarations with knowledge") {
//  this += Transformation("Process new stencil templates", {
//    case stencil : L2_StencilTemplateDecl =>
//      // TODO: how to choose levels?
//      for (level <- Knowledge.levels) {
//        val domain = L2_DomainCollection.getByIdentifier(stencil.domainName).get
//        L2_StencilTemplateCollection.add(L2_StencilTemplate(stencil.name, level, stencil.localization, domain, stencil.offsets)) // defer level determination
//      }
//      stencil // None // consume declaration statement - FIXME: might be part of an operator decl
//  })
//}
