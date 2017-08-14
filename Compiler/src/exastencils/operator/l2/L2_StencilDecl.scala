package exastencils.operator.l2

import exastencils.base.l2.L2_MayBlockResolution
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.logger._

/// L2_StencilDecl

abstract class L2_StencilDecl extends L2_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L2 stencil declaration for stencil $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L2_PrepareStencilDeclaration

object L2_PrepareStencilDeclarations extends DefaultStrategy("Prepare knowledge for L2 stencils") {
  this += Transformation("Process new stencils", {
    case decl : L2_OperatorFromEquation =>
      decl.addDeclarations()
      decl // preserve declaration statement

    case decl : L2_StencilDecl =>
      L2_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L2_ProcessStencilDeclaration

object L2_ProcessStencilDeclarations extends DefaultStrategy("Integrate L2 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case decl : L2_StencilDecl if L2_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
