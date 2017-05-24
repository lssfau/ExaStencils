package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilDecl.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import exastencils.datastructures._
import exastencils.knowledge.|LAYER_LC|._
import exastencils.logger._

/// |LAYER_UC|_StencilDecl

abstract class |LAYER_UC|_StencilDecl extends |LAYER_UC|_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress |LAYER_UC| stencil declaration for stencil $name; this is not supported")
  def addToKnowledge() : Unit
}

/// |LAYER_UC|_PrepareStencilDeclaration

object |LAYER_UC|_PrepareStencilDeclarations extends DefaultStrategy("Prepare knowledge for |LAYER_UC| stencils") {
  this += Transformation("Process new stencils", {
    case decl : |LAYER_UC|_StencilDecl =>
      |LAYER_UC|_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// |LAYER_UC|_ProcessStencilDeclaration

object |LAYER_UC|_ProcessStencilDeclarations extends DefaultStrategy("Integrate |LAYER_UC| stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case decl : |LAYER_UC|_StencilDecl if !|LAYER_UC|_FutureKnowledgeAccess.existsInStmt(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
"""
  }
}
