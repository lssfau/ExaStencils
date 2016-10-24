package exastencils.operator.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.prettyprinting._

/// L4_StencilTemplateDecl

object L4_StencilTemplateDecl {
  def apply(identifier : L4_Identifier, offsets : List[L4_Index]) =
    new L4_StencilTemplateDecl(identifier, offsets.to[ListBuffer])
}

case class L4_StencilTemplateDecl(override var identifier : L4_Identifier, var offsets : ListBuffer[L4_Index]) extends L4_LeveledKnowledgeDecl {
  override def prettyprint(out : PpStream) = {
    out << "StencilTemplate " << identifier << " {\n"
    for (offset <- offsets)
      out << offset << " =>\n"
    out << "}"
  }

  override def addToKnowledge() : Unit = {
    identifier match {
      case L4_BasicIdentifier(name)                          =>
        for (level <- Knowledge.levels) {
          val stencilTemplate = L4_StencilTemplate(name, level, Duplicate(offsets))
          L4_StencilTemplateCollection.add(stencilTemplate)
        }
      case L4_LeveledIdentifier(name, L4_SingleLevel(level)) =>
        val stencilTemplate = L4_StencilTemplate(name, level, offsets)
        L4_StencilTemplateCollection.add(stencilTemplate)
    }
  }
}

/// L4_ProcessStencilTemplateDeclarations

object L4_ProcessStencilTemplateDeclarations extends DefaultStrategy("Integrate Layer4 stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case stencil : L4_StencilTemplateDecl =>
      stencil.addToKnowledge()
      None // consume declaration statement
  })
}
