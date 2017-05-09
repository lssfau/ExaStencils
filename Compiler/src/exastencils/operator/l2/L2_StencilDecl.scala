package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

/// L2_StencilDecl

object L2_StencilDecl {
  def apply(name : String, levels : Option[L2_LevelSpecification], entries : List[L2_StencilEntry]) =
    new L2_StencilDecl(name, levels, entries.to[ListBuffer])
}

case class L2_StencilDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var entries : ListBuffer[L2_StencilEntry]) extends L2_Statement {

  override def prettyprint(out : PpStream) = {
    out << "--- FIXME ---"
  }

  override def progress = Logger.error(s"Trying to progress l2 stencil $name; this is not supported")
}

/// L2_ProcessStencilDeclaration

object L2_ProcessStencilDeclarations extends DefaultStrategy("Integrate Layer2 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case stencil : L2_StencilDecl =>
      val levelList = L2_LevelSpecification.extractLevelListDefAll(stencil.levels)
      for (level <- levelList)
        L2_StencilCollection.add(L2_Stencil(stencil.name, level, stencil.entries)) // defer level determination
      stencil // None // consume declaration statement - FIXME: might be part of an operator decl
  })
}
