package exastencils.operator.l3

import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.logger._

/// L3_StencilDecl

abstract class L3_StencilDecl extends L3_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L3 stencil declaration for stencil $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L3_PrepareStencilDeclaration

object L3_PrepareStencilDeclarations extends DefaultStrategy("Prepare knowledge for L3 stencils") {
  this += Transformation("Process new stencils", {
    case decl : L3_StencilDecl =>
      L3_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L3_ProcessStencilDeclaration

object L3_ProcessStencilDeclarations extends DefaultStrategy("Integrate L3 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case decl : L3_StencilDecl if !L3_FutureKnowledgeAccess.existsInStmt(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
