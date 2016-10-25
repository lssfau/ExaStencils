package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_StencilDecl

object L3_StencilDecl {
  def apply(name : String, entries : List[L3_StencilEntry]) = new L3_StencilDecl(name, entries.to[ListBuffer])
}

case class L3_StencilDecl(var name : String, var entries : ListBuffer[L3_StencilEntry]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "Stencil" << ' ' << name << ' ' << "{\n" <<< (entries, "\n") << "}\n"
  override def progress = Logger.error(s"Trying to progress l3 stencil $name; this is not supported")
}

/// L3_ProcessStencilDeclarations

object L3_ProcessStencilDeclarations extends DefaultStrategy("Integrate Layer3 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case stencil : L3_StencilDecl =>
      for (level <- Knowledge.levels) // TODO: how to choose levels?
        L3_StencilCollection.add(L3_Stencil(stencil.name, level, stencil.entries)) // defer level determination
      None // consume declaration statement

    case stencil : L3_StencilFromDefault =>
      for (level <- Knowledge.levels) // TODO: how to choose levels?
        L3_StencilCollection.add(L3_Stencil(stencil.name, level, stencil.generateEntries())) // defer level determination
      None // consume declaration statement
  })
}
