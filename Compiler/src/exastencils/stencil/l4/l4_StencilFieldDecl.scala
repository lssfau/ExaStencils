package exastencils.stencil.l4

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldCollection
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.prettyprinting.PpStream

case class L4_StencilFieldDecl(
    override var identifier : L4_Identifier,
    var fieldName : String,
    var stencilName : String) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "StencilField " << identifier.name << "< " << fieldName << " => " << stencilName << " >" <<
      "@" << identifier.asInstanceOf[L4_LeveledIdentifier].level << '\n'
  }

  def composeStencilField(level : Int) : L4_StencilField = {
    val resolvedField = L4_FieldCollection.getByIdentifier(fieldName, level).get
    // TODO: val resolvedStencil = L4_StencilTemplateCollection.getByIdentifier(stencilName, level).get
    val resolvedStencil = L4_StencilCollection.getByIdentifier(stencilName, level).get

    // compile final stencil field
    L4_StencilField(identifier.name, level, resolvedField, resolvedStencil)
  }

  override def addToKnowledge() : Unit = {
    identifier match {
      case L4_BasicIdentifier(name)                          =>
        for (level <- Knowledge.levels)
          L4_StencilFieldCollection.add(composeStencilField(level))
      case L4_LeveledIdentifier(name, L4_SingleLevel(level)) =>
        L4_StencilFieldCollection.add(composeStencilField(level))
        None // consume declaration statement
    }
  }
}

/// L4_ProcessStencilDeclarations

object L4_ProcessStencilFieldDeclarations extends DefaultStrategy("Integrating L4 stencil field declarations with knowledge") {
  this += Transformation("Process new stencil fields", {
    case stencilFieldDecl : L4_StencilFieldDecl =>
      stencilFieldDecl.addToKnowledge
      None
  })
}