package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilCollection extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilCollection.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_KnowledgeContainer._
import exastencils.knowledge.|LAYER_LC|._
import exastencils.operator.|NEXT_LC|._

/// |LAYER_UC|_StencilCollection

object |LAYER_UC|_StencilCollection extends |LAYER_UC|_LeveledKnowledgeCollection[|LAYER_UC|_Stencil, |NEXT_UC|_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  |LAYER_UC|_KnowledgeContainer.register(this)

  |LAYER_UC|_PrepareDeclarations.strategies += |LAYER_UC|_PrepareStencilDeclarations
  |LAYER_UC|_ProcessDeclarations.strategies += |LAYER_UC|_ProcessStencilDeclarations

  |LAYER_UC|_PrepareAccesses.strategies += |LAYER_UC|_PrepareStencilAccesses
  |LAYER_UC|_ResolveAccesses.strategies += |LAYER_UC|_ResolveStencilAccesses

  override def name = "|LAYER_UC|_StencilCollection"
  override def progress() = objects.foreach(obj => |NEXT_UC|_StencilCollection.add(obj.progress()))
}
"""
  }
}
