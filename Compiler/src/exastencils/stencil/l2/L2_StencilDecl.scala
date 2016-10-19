package exastencils.stencil.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

///// L2_StencilDecl
//
//case class L2_StencilDecl(var name : String, var entries : ListBuffer[L2_StencilEntry]) extends L2_Statement {
//  override def prettyprint(out : PpStream) = out << "Stencil" << ' ' << name << ' ' << "{\n" <<< (entries, "\n") << "}\n"
//  override def progress = Logger.error(s"Trying to progress l2 stencil $name; this is not supported")
//}
//
///// L2_ProcessStencilDeclaration
//
//object L2_ProcessStencilDeclaration extends DefaultStrategy("Integrate Layer2 stencil declarations with knowledge") {
//  this += Transformation("Process new stencils", {
//    case stencil : L2_StencilDecl => {
//      for (level <- Knowledge.levels) // TODO: how to choose levels?
//        L2_StencilCollection.add(L2_Stencil(stencil.name, level, stencil.entries)) // defer level determination
//      stencil // None // consume declaration statement - FIXME: might be part of an operator decl
//    }
//  })
//}
