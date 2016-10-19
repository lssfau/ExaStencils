package exastencils.stencil.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

///// L3_StencilDecl
//
//case class L3_StencilDecl(var name : String, var entries : ListBuffer[L3_StencilEntry]) extends L3_Statement {
//  override def prettyprint(out : PpStream) = { out << "Stencil" << ' ' << name << ' ' << "{\n" <<< (entries, "\n") << "}\n" }
//  override def progress = { Logger.error(s"Trying to progress l3 stencil $name; this is not supported") }
//}
//
///// L3_ProcessStencilDeclaration
//
//object L3_ProcessStencilDeclaration extends DefaultStrategy("Integrate Layer3 stencil declarations with knowledge") {
//  this += Transformation("Process new stencils", {
//    case stencil : L3_StencilDecl => {
//      for (level <- Knowledge.levels) // TODO: how to choose levels?
//        L3_StencilCollection.add(L3_Stencil(stencil.name, level, stencil.entries)) // defer level determination
//      stencil // None // consume declaration statement - FIXME: might be part of an operator decl
//    }
//  })
//}
