package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.prettyprinting.PpStream

/// L4_StencilDecl

object L4_StencilDecl {
  def apply(identifier : L4_Identifier, entries : L4_StencilEntry*) = new L4_StencilDecl(identifier, entries.to[ListBuffer])
  def apply(identifier : L4_Identifier, entries : List[L4_StencilEntry]) = new L4_StencilDecl(identifier, entries.to[ListBuffer])
}

case class L4_StencilDecl(override var identifier : L4_Identifier, var entries : ListBuffer[L4_StencilEntry]) extends L4_LeveledKnowledgeDecl {
  override def prettyprint(out : PpStream) = {
    out << "Stencil " << identifier.name << '@' << identifier.asInstanceOf[L4_LeveledIdentifier].level << " {\n"
    out <<< (entries, "\n") << '\n'
    out << "}\n"
  }

  override def addToKnowledge() : Unit = {
    identifier match {
      case L4_BasicIdentifier(name)                          =>
        for (level <- Knowledge.levels) {
          val stencil = L4_Stencil(name, level, Duplicate(entries))
          L4_StencilCollection.add(stencil)
        }
      case L4_LeveledIdentifier(name, L4_SingleLevel(level)) =>
        val stencil = L4_Stencil(name, level, entries)
        L4_StencilCollection.add(stencil)
    }
  }
}

/// L4_ProcessStencilDeclarations

object L4_ProcessStencilDeclarations extends DefaultStrategy("Integrating L4 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case stencilDecl : L4_StencilDecl =>
      stencilDecl.addToKnowledge()
      None // consume declaration statement
  })
}
