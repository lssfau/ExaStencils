package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilTemplateCollection extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilTemplateCollection.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_KnowledgeContainer._
import exastencils.knowledge.|LAYER_LC|._
import exastencils.operator.|NEXT_LC|._

/// |LAYER_UC|_StencilTemplateCollection

object |LAYER_UC|_StencilTemplateCollection extends |LAYER_UC|_LeveledKnowledgeCollection[|LAYER_UC|_StencilTemplate, |NEXT_UC|_StencilTemplate] {
  exastencils.core.Duplicate.registerConstant(this)

  |LAYER_UC|_KnowledgeContainer.register(this)

  |LAYER_UC|_PrepareDeclarations.strategies += |LAYER_UC|_PrepareStencilTemplateDeclarations
  |LAYER_UC|_ProcessDeclarations.strategies += |LAYER_UC|_ProcessStencilTemplateDeclarations

//  |LAYER_UC|_PrepareAccesses.strategies += |LAYER_UC|_PrepareStencilTemplateAccesses
//  |LAYER_UC|_ResolveAccesses.strategies += |LAYER_UC|_ResolveStencilTemplateAccesses

  override def name = "|LAYER_UC|_StencilTemplateCollection"
  override def progress() = objects.foreach(obj => |NEXT_UC|_StencilTemplateCollection.add(obj.progress()))
}
"""
  }
}
