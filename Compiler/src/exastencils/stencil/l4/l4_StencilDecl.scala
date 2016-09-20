package exastencils.stencil.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.knowledge.Knowledge
import exastencils.prettyprinting.PpStream

/// L4_StencilDecl

object L4_StencilDecl {
  def apply(identifier : Identifier, entries : L4_StencilEntry*) = new L4_StencilDecl(identifier, entries.to[ListBuffer])
  def apply(identifier : Identifier, entries : List[L4_StencilEntry]) = new L4_StencilDecl(identifier, entries.to[ListBuffer])
}

case class L4_StencilDecl(override var identifier : Identifier, var entries : ListBuffer[L4_StencilEntry]) extends L4_KnowledgeDeclStatement with HasIdentifier {
  override def prettyprint(out : PpStream) = {
    out << "Stencil " << identifier.name << '@' << identifier.asInstanceOf[LeveledIdentifier].level << " {\n"
    out <<< (entries, "\n") << '\n'
    out << "}\n"
  }

  override def addToKnowledge() = {
    identifier match {
      case BasicIdentifier(name)                                    =>
        for (level <- Knowledge.levels) {
          val stencil = L4_Stencil(name, level, Duplicate(entries))
          L4_StencilCollection.add(stencil)
        }
      case LeveledIdentifier(name, SingleLevelSpecification(level)) =>
        val stencil = L4_Stencil(name, level, entries)
        L4_StencilCollection.add(stencil)
        None // consume declaration statement
    }
  }
}

/// L4_ProcessStencilDeclarations

object L4_ProcessStencilDeclarations extends DefaultStrategy("Integrating L4 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case stencilDecl : L4_StencilDecl =>
      stencilDecl.addToKnowledge
      None
  })
}
